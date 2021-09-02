{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Text.Pandoc.ISO.Types where

import Text.Pandoc.Definition

import Data.Ord (comparing)
import Data.Hashable
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Control.Lens



class Inlinable a where
    -- | Convert object into a list of Pandoc 'Inline' values.
    inline :: a -> [Inline]


-- | Little-endian clause number,
-- boolean indicates whether it's an annex or not
data ClauseNum = ClauseNum Int [Int] Bool deriving Eq

type Identifier = T.Text

prependToBlocks :: [Inline] -> [Inline] -> [Block] -> [Block]
prependToBlocks toPrepend connector blks = case blks of
    [] -> [Plain toPrepend]
    (Plain ins:bs) -> (Plain (connected ins):bs)
    (Para ins:bs) -> (Para (connected ins):bs)
    blocks -> (Plain toPrepend:blocks)
    where connected ins = toPrepend ++ connector ++ ins


-- | Turn a 'ClauseNum' object into a big-endian list
clauseAsList :: ClauseNum -> [Int]
clauseAsList (ClauseNum x xs _) = reverse (x:xs)

clauseLevel :: ClauseNum -> Int
clauseLevel (ClauseNum _ xs _) = 1 + length xs

isAnnex :: ClauseNum -> Bool
isAnnex (ClauseNum _ _ x) = x


-- | Descend into the first subclause of a clause
-- If 'ClauseNum' is 'Nothing', it is treated as the (unnumbered)
-- root clause.
descendInto :: Maybe ClauseNum -> ClauseNum
descendInto parent = ClauseNum 1 tailPart annex
    where (tailPart, annex) = case parent of
            Nothing -> ([], False)
            Just (ClauseNum x xs anx) -> ((x:xs), anx)


-- | Move to next clause at the same level
nextClause :: ClauseNum -> ClauseNum
nextClause (ClauseNum x xs annex) = ClauseNum (x + 1) xs annex


-- | Move to superclause
superclause :: ClauseNum -> Maybe ClauseNum
superclause (ClauseNum _ [] _) = Nothing
superclause (ClauseNum _ (x:xs) annex) = Just $ ClauseNum x xs annex

-- | Move up a number of levels in the hierarchy (returning 'Nothing'
-- if the clause is not deep enough)
goUp :: Int -> ClauseNum -> Maybe ClauseNum
goUp 0 cn = Just cn
goUp nos cn =  superclause cn >>= goUp (nos - 1)


clauseNumText :: ClauseNum -> T.Text
clauseNumText cls = T.intercalate "." $ case isAnnex cls of
        False -> numArrText nums
        True -> T.singleton (ltrPrefix $ head nums):numArrText (tail nums)
    where nums = clauseAsList cls
          numArrText = fmap (T.pack . show)
          ltrPrefix num = toEnum $ 0x40 + num

clauseNumToIdent :: Maybe ClauseNum -> Identifier
clauseNumToIdent Nothing = "root"
clauseNumToIdent (Just cls) = T.intercalate "-" nums
    where nums = fmap (T.pack . show) $ clauseAsList cls

instance Show ClauseNum where
    show = T.unpack . clauseNumText


instance Hashable ClauseNum where
    hashWithSalt = hashUsing $ \(ClauseNum x xs annex) -> (x, xs, annex)


instance Ord ClauseNum where
    compare = comparing $ \x -> (clauseAsList x, isAnnex x)
    

data ClauseInfo = ClauseInfo
    { clauseNum   :: ClauseNum
    , clauseTitle :: [Inline]
    , clauseLabel :: Identifier } deriving Show


instance Inlinable ClauseInfo where
    inline (ClauseInfo num title _) = 
        [ Str $ clauseNumText num, Str ",", Space, Str "\"" ] ++ title ++ [Str "\""]


data BibRefInfo = BibRefInfo 
    { bibRefLabel  :: Identifier
    , bibDispLabel :: [Inline] } deriving Show


instance Inlinable BibRefInfo where
    inline = bibDispLabel


data Captioned = Captioned
    { captionedNum :: Int
    , caption :: Caption
    , captionedLabel :: Identifier
    , captionedInClause :: ClauseNum } deriving Show


instance Inlinable Captioned where
    inline x = case caption x of
            -- Use short caption if available
            Caption (Just shortCaption) _ -> shortCaption
            -- Attempt to concat paras and plains
            -- (best effort basis)
            Caption _ blocks -> concatMap grabInlines blocks
                where grabInlines (Plain inls) = inls
                      grabInlines (Para inls) = inls
                      grabInlines _ = []

type DocRefs = HM.HashMap Identifier


class NoteLikeType a where
    noteLikePrefix :: a -> T.Text
    identifierPrefix :: a -> T.Text
    identifiersInClause :: a -> Lens' RefBuildingState [Identifier]
    knownInstances :: a -> Lens' DocRefStore (DocRefs (NoteLike a))

-- Using singletons here for now, to keep the number of lang extensions
-- required to a minimum (in particular things like flexible/overlapping
-- instances)

data NoteLike a = NoteLike a (Maybe Int)
data ISONote = ISONote
data ISOExample = ISOExample

instance NoteLikeType ISONote where
    noteLikePrefix _ = "NOTE"
    identifierPrefix _ = "not"
    identifiersInClause _ = currentNotes
    knownInstances _ = noteRefs

instance NoteLikeType ISOExample where
    noteLikePrefix _ = "EXAMPLE"
    identifierPrefix _ = "exa"
    identifiersInClause _ = currentExamples
    knownInstances _ = exampleRefs

edNotePrefix :: T.Text
edNotePrefix = "ED NOTE"

instance NoteLikeType a => Inlinable (NoteLike a) where
    inline (NoteLike noteType num) = case num of
            Nothing -> [Str pref]
            Just n -> [Str $ pref <> " " <> T.pack (show n)]
        where pref = noteLikePrefix noteType


data DocRefStore = DocRefStore
    { _clauseRefs :: DocRefs ClauseInfo
    , _tableRefs :: DocRefs Captioned
    , _noteRefs :: DocRefs (NoteLike ISONote)
    , _exampleRefs :: DocRefs (NoteLike ISOExample)
    , _normRefs :: DocRefs BibRefInfo }


initRefStore :: DocRefStore
initRefStore = DocRefStore HM.empty HM.empty HM.empty HM.empty HM.empty


clauseRefs :: Lens' DocRefStore (DocRefs ClauseInfo)
clauseRefs wrap ctx = doSet <$> wrap (_clauseRefs ctx)
    where doSet x = ctx { _clauseRefs = x }

tableRefs :: Lens' DocRefStore (DocRefs Captioned)
tableRefs wrap ctx = doSet <$> wrap (_tableRefs ctx)
    where doSet x = ctx { _tableRefs = x }

noteRefs :: Lens' DocRefStore (DocRefs (NoteLike ISONote))
noteRefs wrap ctx = doSet <$> wrap (_noteRefs ctx)
    where doSet x = ctx { _noteRefs = x }

exampleRefs :: Lens' DocRefStore (DocRefs (NoteLike ISOExample))
exampleRefs wrap ctx = doSet <$> wrap (_exampleRefs ctx)
    where doSet x = ctx { _exampleRefs = x }

normRefs :: Lens' DocRefStore (DocRefs BibRefInfo)
normRefs wrap ctx = doSet <$> wrap (_normRefs ctx)
    where doSet x = ctx { _normRefs = x }


data RefBuildingState = RefBuildingState
    { _currentRefs :: DocRefStore
    , _currentClause :: Maybe ClauseNum
    , _currentNotes :: [Identifier]
    , _currentExamples :: [Identifier]
    , _lastTable :: Int }


initRefBuildingState :: RefBuildingState
initRefBuildingState = RefBuildingState
    { _currentRefs = initRefStore 
    , _currentClause = Nothing
    , _currentNotes = []
    , _currentExamples = []
    , _lastTable = 0 }


currentRefs :: Lens' RefBuildingState DocRefStore
currentRefs wrap ctx = doSet <$> wrap (_currentRefs ctx)
    where doSet x = ctx { _currentRefs = x }

currentClause :: Lens' RefBuildingState (Maybe ClauseNum)
currentClause wrap ctx = doSet <$> wrap (_currentClause ctx)
    where doSet x = ctx { _currentClause = x }

currentNotes :: Lens' RefBuildingState [Identifier]
currentNotes wrap ctx = doSet <$> wrap (_currentNotes ctx)
    where doSet x = ctx { _currentNotes = x }

currentExamples :: Lens' RefBuildingState [Identifier]
currentExamples wrap ctx = doSet <$> wrap (_currentExamples ctx)
    where doSet x = ctx { _currentExamples = x }

lastTable :: Lens' RefBuildingState Int
lastTable wrap ctx = doSet <$> wrap (_lastTable ctx)
    where doSet x = ctx { _lastTable = x }
