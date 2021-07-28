{-# LANGUAGE OverloadedStrings, FlexibleContexts, RankNTypes #-}

module Text.Pandoc.ISO
    ( handleInternalRefs, RefError 
    , handleStyles )
    where

import Text.Pandoc.Definition
import Text.Pandoc.Walk

import Data.Ord (comparing)
import Data.Maybe (isJust, isNothing, catMaybes, fromMaybe)
import Data.Foldable (find)
import Data.List (intercalate)
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


-- Internal helper for processNoteLikes.
-- Observe: we pass in an explicit type witness here.
processNoteLikes' :: (Monad m, NoteLikeType a) => a -> StateT RefBuildingState (ExceptT RefError m) ()
processNoteLikes' noteLikeType = do
        notes <- use inClause 
        inClause .= []
        let inserts = uncurry HM.insert <$> numberNotes notes
        -- insert the notes
        currentRefs . known %= foldr (.) id inserts
    where -- no NOTE number if there's only one
          numberNotes [singleNote] = [(singleNote, createNote Nothing)]
          numberNotes notes = zip (reverse notes) (createNote . Just <$> [1..])
          createNote = NoteLike noteLikeType
          -- necessary to prevent the monomorphism restriction (?) from kicking in
          inClause :: Lens' RefBuildingState [Identifier]
          inClause = identifiersInClause noteLikeType
          known = knownInstances noteLikeType
          


-- | Helper function to process "note-like" thinks (actual notes, examples)
-- at the end of a clause.
processNoteLikes :: Monad m => StateT RefBuildingState (ExceptT RefError m) ()
processNoteLikes = processNoteLikes' ISONote >> processNoteLikes' ISOExample


-- | Extract reference targets from block elements and reformat their
-- contents if necessary.
processBlock :: Monad m => Block 
             -> StateT RefBuildingState (ExceptT RefError m) Block
processBlock blk@(Header lvl attrs headerText) = do
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
                else processNoteLikes >> endClause cls diff
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

processBlock blk@(Table attrs tblCapt cs th tb tf) = do
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

processBlock blk@(Div (divId', classes, kvals) blks)
    | "note" `elem` classes = registerNoteLike ISONote
    | "example" `elem` classes = registerNoteLike ISOExample
    | "normref" `elem` classes = registerNormativeReference
    | "ed-note" `elem` classes = prependEdNote
    | otherwise = return blk
    where colon = [Str ":", Space]
          registerNoteLike noteLikeType = do
                num <- uses inClause ((+1) . length)
                curClause <- uses currentClause clauseNumToIdent 
                -- derive an ID if none exists yet
                divId <- lift $ case divId' of
                    "" -> return $ idPref <> ":" <> curClause <> "_" <> T.pack (show num)
                    -- TODO maybeToErr?
                    _ -> case checkForPrefix idPref divId' of
                        Nothing -> throwError (WrongPrefix idPref divId')
                        _ -> return divId'
                -- register the note ID
                inClause %= (divId:)
                -- update the block element with a cite node
                -- This will later get expanded to "NOTE n" or "EXAMPLE n"
                -- as necessary
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
            where idPref = identifierPrefix noteLikeType
                  -- necessary to prevent the monomorphism restriction (?) from kicking in
                  inClause :: Lens' RefBuildingState [Identifier]
                  inClause = identifiersInClause noteLikeType
          findAttr key = fmap snd . find (\attr -> fst attr == key)
          registerNormativeReference = do
            -- check for 'nrm:' prefix
            lift $ case checkForPrefix "nrm" divId' of
                Nothing -> throwError (WrongPrefix "nrm" divId')
                _ -> return ()
                
            -- find the reference's label attribute, or use the div's id attribute
            -- if not present (as a backup)
            let dispLabel = fromMaybe divId' (findAttr "label" kvals)

            -- register the info entry
            currentRefs . normRefs . at divId' .= Just (BibRefInfo divId' [Str dispLabel])
            return blk
          prependEdNote = return $ Div (divId', classes, kvals) newBlks
            where newBlks = prependToBlocks [Str edNotePrefix] colon blks
            

processBlock x = return x


-- | Walk the tree and gather up all referenceable objects.
-- This also adjusts table captions, hence why it returns a new Pandoc object
-- as well.
processBlocks :: Monad m => Pandoc -> ExceptT RefError m (Pandoc, DocRefStore)
processBlocks doc = do
     let sniffer = walkPandocM processBlock doc <* processNoteLikes
     (newDoc, finalState) <- runStateT sniffer initRefBuildingState
     return (newDoc, finalState ^. currentRefs)


-- | Helper functions to find references.
findAndFormatRef :: Monad m => Identifier -> DocRefs a 
                 -> (Maybe a -> b) -> StateT [Identifier] m b
findAndFormatRef refId refStore fmt = do
    let theRef = refStore ^. at refId
    when (isNothing theRef) $ modify (refId:)
    return (fmt theRef)


-- | Process one of our "special" inline citations, and accumulate undefined identifiers
-- in the process. Returns 'Nothing' if the classifier prefix is not present or not recognised.
processInlineCitation :: Monad m => DocRefStore -> Citation -> Maybe (StateT [Identifier] m Inline)
processInlineCitation refs cited = case refPrefix of
        Just "sec" -> Just $ dispatchTo clauseRefs $ fmtInfo linkify "clause"
        Just "tbl" -> Just $ dispatchTo tableRefs $ fmtInfo linkify "table"
        Just "not" -> Just $ dispatchTo noteRefs $ fmtInfo spanify "note"
        Just "exa" -> Just $ dispatchTo exampleRefs $ fmtInfo spanify "example"
        Just "nrm" -> Just $ dispatchTo normRefs $ fmtInfo citationLink "normative reference"
        _ -> Nothing
    where refId = citationId cited
          refPrefix = extractClassifierPrefix refId
          dispatchTo theLens = findAndFormatRef refId (refs ^. theLens)
          fmtInfo _ refType Nothing = Strong [Str errStr]
            where errStr = "!No " <> refType <> " labelled " <> refId <> "!"
          fmtInfo transf _ (Just info) = transf (inline info)
          linkify text = Link ("", [], []) text (T.cons '#' refId, "")
          spanify text = Span ("", [], []) text
          citationLink text = spanify $ pref ++ [linkify text] ++ suff
            where pref = citationPrefix cited
                  suff = citationSuffix cited


substituteInlineRefs :: Monad m => DocRefStore -> Inline 
                     -> StateT [Identifier] (ExceptT RefError m) Inline
-- do nothing if we can't process the citation
substituteInlineRefs refs inl@(Cite [cited] _) = fromMaybe (return inl) maybeHandle
    where maybeHandle = processInlineCitation refs cited 

-- handler for Cite inlines with multiple members, intended for use with normative refs
substituteInlineRefs refs inl@(Cite cites _)
    -- does any of the refs have "nrm:" as its prefix?
    | any isNormRef cites = case find (not . isNormRef) cites of
        -- can't mix normative refs with other refs, since it messes up the interaction
        -- between our normative reference handler and citeproc.
        Just cite -> throwError (WrongPrefix "nrm" (citationId cite))
        -- the fromMaybe should be unnecessary here, but let's code defensively
        _ -> fromMaybe (return inl) maybeHandleAll
    -- if no normative refs present -> ignore
    | otherwise = return inl
    where isNormRef cited = isJust (checkForPrefix "nrm" $ citationId cited)
          -- the passage over Maybe is purely defensive here, since we already
          -- checked for nrm: everywhere, but we still use processInlineCitation
          -- for uniformity's sake.
          maybeHandleAll = do
            -- process each Citation object individually
            processedRefs <- sequence $ processInlineCitation refs <$> cites
            -- ... and put the results in a Span
            return (joinCitations <$> sequence processedRefs)
          joinCitations :: [Inline] -> Inline
          joinCitations = Span ("", [], []) . intercalate [Str ";", Space] . fmap return

substituteInlineRefs _ inl = return inl



handleInternalRefs :: Monad m => Pandoc -> ExceptT RefError m (Pandoc, [Identifier])
handleInternalRefs doc = do
    (newDoc, refs) <- processBlocks doc
    let inlineSub = substituteInlineRefs refs
    runStateT (walkPandocM inlineSub newDoc) []


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

