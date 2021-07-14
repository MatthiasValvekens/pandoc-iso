{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Text.Pandoc.ISO
    ( handleInternalRefs, RefError 
    , handleStyles )
    where

import Text.Pandoc.Definition
import Text.Pandoc.Walk

import Data.Ord (comparing)
import Data.Maybe (isNothing, catMaybes)
import Data.Hashable
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Lens



class Inlinable a where
    -- | Convert object into a list of Pandoc 'Inline' values.
    inline :: a -> [Inline]


-- | Little-endian clause number
data ClauseNum = ClauseNum Int [Int] deriving Eq

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
clauseAsList (ClauseNum x xs) = reverse (x:xs)

clauseLevel :: ClauseNum -> Int
clauseLevel (ClauseNum _ xs) = 1 + length xs


-- | Descend into the first subclause of a clause
-- If 'ClauseNum' is 'Nothing', it is treated as the (unnumbered)
-- root clause.
descendInto :: Maybe ClauseNum -> ClauseNum
descendInto parent = ClauseNum 1 $ case parent of
    Nothing -> []
    Just (ClauseNum x xs) -> (x:xs)


-- | Move to next clause at the same level
nextClause :: ClauseNum -> ClauseNum
nextClause (ClauseNum x xs) = ClauseNum (x + 1) xs


-- | Move to superclause
superclause :: ClauseNum -> Maybe ClauseNum
superclause (ClauseNum _ []) = Nothing
superclause (ClauseNum _ (x:xs)) = Just $ ClauseNum x xs

-- | Move up a number of levels in the hierarchy (returning 'Nothing'
-- if the clause is not deep enough)
goUp :: Int -> ClauseNum -> Maybe ClauseNum
goUp 0 cn = Just cn
goUp nos cn =  superclause cn >>= goUp (nos - 1)


clauseNumText :: ClauseNum -> T.Text
clauseNumText cls = T.intercalate "." nums
    where nums = fmap (T.pack . show) $ clauseAsList cls


clauseNumToIdent :: Maybe ClauseNum -> Identifier
clauseNumToIdent Nothing = "root"
clauseNumToIdent (Just cls) = T.intercalate "-" nums
    where nums = fmap (T.pack . show) $ clauseAsList cls

instance Show ClauseNum where
    show = T.unpack . clauseNumText


instance Hashable ClauseNum where
    hashWithSalt = hashUsing $ \(ClauseNum x xs) -> (x, xs)


instance Ord ClauseNum where
    compare = comparing clauseAsList
    


data ClauseInfo = ClauseInfo
    { clauseNum :: ClauseNum
    , clauseTitle :: [Inline]
    , clauseLabel :: Identifier } deriving Show


instance Inlinable ClauseInfo where
    inline (ClauseInfo num title _) = 
        [ Str $ clauseNumText num, Str ",", Space, Str "\"" ] ++ title ++ [Str "\""]


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

newtype NoteNum = NoteNum (Maybe Int)

instance Inlinable NoteNum where
    inline (NoteNum Nothing) = [Str "NOTE"]
    inline (NoteNum (Just n)) = [Str $ "NOTE " <> T.pack (show n)]

data DocRefStore = DocRefStore
    { _clauseRefs :: DocRefs ClauseInfo
    , _tableRefs :: DocRefs Captioned
    , _noteRefs :: DocRefs NoteNum }


initRefStore :: DocRefStore
initRefStore = DocRefStore HM.empty HM.empty HM.empty


clauseRefs :: Lens' DocRefStore (DocRefs ClauseInfo)
clauseRefs wrap ctx = doSet <$> wrap (_clauseRefs ctx)
    where doSet x = ctx { _clauseRefs = x }

tableRefs :: Lens' DocRefStore (DocRefs Captioned)
tableRefs wrap ctx = doSet <$> wrap (_tableRefs ctx)
    where doSet x = ctx { _tableRefs = x }

noteRefs :: Lens' DocRefStore (DocRefs NoteNum)
noteRefs wrap ctx = doSet <$> wrap (_noteRefs ctx)
    where doSet x = ctx { _noteRefs = x }


data RefBuildingState = RefBuildingState
    { _currentRefs :: DocRefStore
    , _currentClause :: Maybe ClauseNum
    , _currentNotes :: [T.Text]
    , _lastTable :: Int }


initRefBuildingState :: RefBuildingState
initRefBuildingState = RefBuildingState
    { _currentRefs = initRefStore 
    , _currentClause = Nothing
    , _currentNotes = []
    , _lastTable = 0 }


currentRefs :: Lens' RefBuildingState DocRefStore
currentRefs wrap ctx = doSet <$> wrap (_currentRefs ctx)
    where doSet x = ctx { _currentRefs = x }

currentClause :: Lens' RefBuildingState (Maybe ClauseNum)
currentClause wrap ctx = doSet <$> wrap (_currentClause ctx)
    where doSet x = ctx { _currentClause = x }

currentNotes :: Lens' RefBuildingState [T.Text]
currentNotes wrap ctx = doSet <$> wrap (_currentNotes ctx)
    where doSet x = ctx { _currentNotes = x }

lastTable :: Lens' RefBuildingState Int
lastTable wrap ctx = doSet <$> wrap (_lastTable ctx)
    where doSet x = ctx { _lastTable = x }


-- | Extract the classifier prefix of a (block's) identifier, if present.
-- Examples: 'sec', 'tbl', 'fig', 'eqn'...
extractClassifierPrefix :: Identifier -> Maybe T.Text
extractClassifierPrefix idStr = do
    -- pick off the first four characters
    (prefix, fourthChar) <- T.unsnoc (T.take 4 idStr)
    -- check if the fourth char was a colon
    guard $ fourthChar == ':'
    return prefix


checkForPrefix :: T.Text -> Identifier -> Maybe Identifier
checkForPrefix expectedPrefix idStr = do
    prefix <- extractClassifierPrefix idStr
    guard $ prefix == expectedPrefix
    return idStr


extractIdWithPrefix :: T.Text -> Attr -> Maybe Identifier
extractIdWithPrefix pref (idStr, _, _) = checkForPrefix pref idStr


-- | Look for a table ID in the caption (a 'Str' of the form "{#tbl:...}"),
-- strip it if necessary, and return the new caption together
-- with the identifier (if found)
-- We only track the first output, but all ID-like inlines will be stripped regardless.
findTableIdInCaption :: Caption -> Writer (Maybe Identifier) Caption
findTableIdInCaption = walkCaptionM findInInlines
    where --findInInlines scans (and possibly modifies) a list of inlines
          findInInlines inls = traverse lookIn inls >>= return . catMaybes
          --lookIn inspects a single inline to check whether it looks like a table ID
          lookIn inl@(Str txt) = case inspectText txt of
                Nothing -> return (Just inl)
                -- Edge case: this will select the first ID if there
                -- there are multiple ID-like elements in the inline,
                -- (due to the Monoid instance on Maybe)
                -- but still strip all such elements from the list.
                -- Don't care about that one too much, but if necessary
                -- we can deal with it by using lists instead of Maybe
                -- and adding error checking.
                Just ident -> tell (Just ident) >> return Nothing
          lookIn inl = return (Just inl)
          inspectText txt = do
            (fstChar, rest) <- T.uncons txt
            guard $ fstChar == '{'
            (sndChar, rest') <- T.uncons rest
            guard $ sndChar == '#'
            (stripped, lstChar) <- T.unsnoc rest'
            guard $ lstChar == '}'
            checkForPrefix "tbl" stripped



data RefError = LevelSkipped (Maybe ClauseNum) 
              | NoClause Identifier
              | WrongPrefix T.Text Identifier
              deriving Show


-- | Throw exception on 'Nothing'
errIfNothing :: MonadError e m => e -> Maybe a -> m a
errIfNothing err Nothing = throwError err
errIfNothing _ (Just x) = return x


-- | Helper function to process notes at the end of a clause
processNotes :: Monad m => StateT RefBuildingState (ExceptT RefError m) ()
processNotes = do
        notes <- use currentNotes
        currentNotes .= []
        let inserts = uncurry HM.insert <$> numberNotes notes
        -- insert the notes
        currentRefs . noteRefs %= foldr (.) id inserts
    where -- no NOTE number if there's only one
          numberNotes [singleNote] = [(singleNote, NoteNum Nothing)]
          numberNotes notes = zip (reverse notes) (NoteNum . Just <$> [1..])

-- | Extract reference targets from block elements.
sniffRef :: Monad m => Block 
           -> StateT RefBuildingState (ExceptT RefError m) Block
sniffRef blk@(Header lvl attrs headerText) = do
    -- first, adjust the current clause
    cls <- use currentClause
    let curLvl = maybe 0 clauseLevel cls
    newClause <- currentClause <<~ case (curLvl - lvl) of
        -- new level is 1 higher -> descend
        (-1) -> return $ Just (descendInto cls)
        -- level is <-1 or positive
        diff -> if diff < 0 
                -- descending more than one level at a time
                -- is not allowed
                then throwError (LevelSkipped cls)
                else processNotes >> endClause cls diff
    -- does the clause have a proper label/identifier?
    -- if so, register it (we sequence over Maybe to do the checks)
    sequence_ $ do
        -- New clause can't be Nothing, but let's be defensive
        newCls <- newClause
        clauseId <- extractIdWithPrefix "sec" attrs
        return (registerClause newCls clauseId)
    return blk
    where -- go up the indicated number of levels, then
          -- increment the clause
          endClause cls diff = return $ fmap nextClause (cls >>= goUp diff)
          registerClause cls clauseId = do
            let clauseInfo = ClauseInfo cls headerText clauseId
            currentRefs . clauseRefs . at clauseId .= Just clauseInfo

sniffRef blk@(Table attrs tblCapt cs th tb tf) = do
        let (cleanedCapt, maybeTableId) = runWriter findTableId
        case maybeTableId of
            Nothing -> return blk -- nothing to do
            -- register the table and update the caption
            Just tableId -> do
                updatedCapt <- registerTable tableId cleanedCapt
                return (Table attrs updatedCapt cs th tb tf)
    where registerTable tableId capt = do
            -- retrieve current clause (throw error if not in a clause)
            cls <- use currentClause >>= errIfNothing (NoClause tableId)
            -- increment table num & retrieve the result
            curTblNum <- lastTable <+= 1
            let newCapt = prependNum curTblNum capt
            let tblInfo = Captioned curTblNum newCapt tableId cls
            currentRefs . tableRefs . at tableId .= Just tblInfo
            return newCapt
          
          findTableId = case extractIdWithPrefix "tbl" attrs of
                Nothing -> findTableIdInCaption tblCapt
                Just x -> tell (Just x) >> return tblCapt
          -- Prepend "Table xx --- " to the table caption
          prependNum num (Caption sh long) = Caption (fmap (tabStr++) sh) long'
            where tabStr' = Str $ "Table " <> T.pack (show num)
                  emdash = [Space, Str "—", Space ]
                  tabStr = (tabStr':emdash)
                  long' = prependToBlocks [tabStr'] emdash long

sniffRef blk@(Div (divId', classes, kvals) blks)
    | "note" `elem` classes = registerNote
    | otherwise = return blk
    where colon = [Str ":", Space]
          registerNote = do
            num <- uses currentNotes ((+1) . length)
            curClause <- uses currentClause clauseNumToIdent 
            -- derive an ID if none exists yet
            divId <- lift $ case divId' of
                "" -> return $ "not:" <> curClause <> "_" <> T.pack (show num)
                -- TODO maybeToErr?
                _ -> case checkForPrefix "not" divId' of
                    Nothing -> throwError (WrongPrefix "not" divId')
                    _ -> return divId'
            -- register the note ID
            currentNotes %= (divId:)
            -- update the block element with a cite node
            let cite = Citation { citationId = divId
                                , citationPrefix = []
                                , citationSuffix = []
                                , citationMode = NormalCitation
                                -- probably not semantically correct
                                -- but this node will be removed by our
                                -- citation processing logic in the next pass
                                -- so it won't be seen by any other filters
                                , citationNoteNum = num
                                , citationHash = 0 }
            let newBlks = prependToBlocks [Cite [cite] []] colon blks
            return $ Div (divId, classes, kvals) newBlks

sniffRef x = return x


-- | Walk the tree and gather up all referenceable objects.
-- This also adjusts table captions, hence why it returns a new Pandoc object
-- as well.
sniffRefs :: Monad m => Pandoc -> ExceptT RefError m (Pandoc, DocRefStore)
sniffRefs doc = do
     let sniffer = walkPandocM sniffRef doc
     (newDoc, finalState) <- runStateT sniffer initRefBuildingState
     return (newDoc, finalState ^. currentRefs)


findAndFormatRef :: Monad m => Identifier -> DocRefs a 
                 -> (Maybe a -> b) -> StateT [Identifier] m b
findAndFormatRef refId refStore fmt = do
    let theRef = refStore ^. at refId
    when (isNothing theRef) $ modify (refId:)
    return (fmt theRef)


substituteInlineRefs :: Monad m => DocRefStore -> Inline -> StateT [Identifier] m Inline
substituteInlineRefs refs inl@(Cite [cited] _) = case refPrefix of
        Just "sec" -> dispatchTo clauseRefs $ fmtInfo linkify "clause"
        Just "tbl" -> dispatchTo tableRefs $ fmtInfo linkify "table"
        Just "not" -> dispatchTo noteRefs $ fmtInfo spanify "note"
        _ -> return inl
    where refId = citationId cited
          refPrefix = extractClassifierPrefix refId
          dispatchTo theLens = findAndFormatRef refId (refs ^. theLens)
          fmtInfo _ refType Nothing = Strong [Str errStr]
            where errStr = "!No " <> refType <> " labelled " <> refId <> "!"
          fmtInfo transf _ (Just info) = transf (inline info)
          linkify text = Link ("", [], []) text (T.cons '#' refId, "")
          spanify text = Span ("", [], []) text

substituteInlineRefs _ inl = return inl


handleInternalRefs :: Monad m => Pandoc -> ExceptT RefError m (Pandoc, [Identifier])
handleInternalRefs doc = do
    (newDoc, refs) <- sniffRefs doc
    let inlineSub = substituteInlineRefs refs
    lift $ runStateT (walkPandocM inlineSub newDoc) []


docxDivStyles :: HM.HashMap T.Text T.Text
docxDivStyles = HM.fromList
    [ ("note", "Note")
    , ("example", "Note")
    , ("note-to-entry", "Note") -- TODO: implement auto-numbering for these
    , ("ed-note", "Editors Note") ]

-- use the first class that makes sense as a word style
handleStyles' :: HM.HashMap T.Text T.Text -> Attr -> Attr
handleStyles' styles (elId, classes, kvals) = (elId, classes, newKvals)
    where asWordStyles = [HM.lookup cls styles | cls <- classes] 
          newKvals = case msum asWordStyles of
            Just style -> (("custom-style", style):kvals)
            Nothing -> kvals

handleStyles :: Monad m => Pandoc -> m Pandoc
handleStyles = walkPandocM (return . blockSub)
    where blockSub (Div attrs blks) = Div attrs' blks
            where attrs' = handleStyles' docxDivStyles attrs
          blockSub x = x

