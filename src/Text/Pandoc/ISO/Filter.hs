{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Text.Pandoc.ISO.Filter
    ( handleInternalRefs
    , processMacros
    , handleStyles 
    , rearrangeMeta
    , RefError (..), OOXMLMacroError (..))
    where

import Data.Maybe (isJust, isNothing, catMaybes, fromMaybe)
import Data.Foldable (find)
import Data.List (intercalate)

import Text.Pandoc.Definition
import Text.Pandoc.Walk

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M

import Control.Monad.Reader
import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Lens

import Text.Pandoc.ISO.Types
import Text.Pandoc.ISO.OOXML (expandOOXMLMacro, ooxmlInline, OOXMLMacroError (..))



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


stripWhiteSpace :: [Inline] -> [Inline]
stripWhiteSpace inls = poststrip (prestrip inls)
    where isWS x = case x of
            Space -> True
            LineBreak -> True
            SoftBreak -> True
            _ -> False
          prestrip [] = []
          prestrip xs@(x:xs')
            | isWS x = prestrip xs'
            | otherwise = xs
          poststrip = reverse . prestrip . reverse


-- | Look for a table ID in the caption (a 'Str' of the form "{#tbl:...}"),
-- strip it if necessary, and return the new caption together
-- with the identifier (if found)
-- We only track the first output, but all ID-like inlines will be stripped regardless.
findTableIdInCaption :: Caption -> Writer (Maybe Identifier) Caption
findTableIdInCaption = walkCaptionM findInInlines 
                        >=> walkCaptionM (return . stripWhiteSpace)
    where --findInInlines scans (and possibly modifies) a list of inlines
          findInInlines inls = traverse lookIn inls <&> catMaybes
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


setId :: T.Text -> Attr -> Attr
setId newId (_, cls, kvals) = (newId, cls, kvals)


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


-- | Format an annex header with the proper styles
fmtAnnex :: Int -> Attr -> [Inline] -> Block
fmtAnnex lvl (elId, elCls, kvals) headerText = Div (elId, elCls, kvals') content
    where style
            | lvl == 1 = "ANNEX"
            | otherwise = T.pack ('a':show lvl)
          kvals' = (("custom-style", style):kvals)
          content'
            | lvl == 1 = (LineBreak:Str annexType:LineBreak:LineBreak:headerText)
            | otherwise = headerText
          content = [Para content']
          annexType
            | "normative" `elem` elCls = "(normative)"
            | otherwise = "(informative)"

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

    -- if we're in the annexes, we need to emit something other than Header
    -- to get the styles right in the ISO template
    return $ if (maybe False isAnnex newClause)
                then fmtAnnex lvl attrs headerText else blk
    where -- go up the indicated number of levels, then
          -- increment the clause
          endClause cls diff = return $ fmap nextClause (cls >>= goUp diff)
          registerClause cls clauseId = do
            let clauseInfo = ClauseInfo cls headerText clauseId
            currentRefs . clauseRefs . at clauseId .= Just clauseInfo

-- Number T&D's within the clause in which they appear
-- (As of now these aren't referenceable at the AST level, we just provide the numbers
-- because ISO wants us to)
processBlock (DefinitionList tnds) = do
        cls <- use currentClause
        let numberTerm = addNum (maybe "" clauseNumText cls)
        return (DefinitionList $ uncurry numberTerm <$> withNums tnds)
    where addNum prefix termNum (term, defs) = ((termNum':LineBreak:term), defs)
            where termNum' = Str $ prefix <> "." <> (T.pack $ show termNum)
          withNums = zip [(1 :: Int)..]

processBlock (Table attrs tblCapt cs th tb tf) = do
        -- Pandoc's Markdown grid table reader tends to put in a lot
        -- of spurious soft breaks. This logic replaces those with regular spaces.
        -- (Resaving the resulting Docx in Word has the same effect, as it happens)
        tb' <- traverse (walkTableBodyM $ return . stripSoftBreaks) tb
        -- next, process the caption
        let (cleanedCapt, maybeTableId) = runWriter findTableId
        (tableId, tblCapt') <- case maybeTableId of
            Nothing -> return ("", tblCapt) -- nothing to do
            -- register the table and update the caption
            Just id' -> (\x -> (id',x)) <$> registerTable id' cleanedCapt
        let attrs' = setId tableId attrs
        -- wrap the table in a Div, to make sure that Pandoc generates
        -- a bookmark for us
        return $ Div attrs' [Table ("", [], []) tblCapt' cs th tb' tf]
    where registerTable tableId capt = do
            -- retrieve current clause (throw error if not in a clause)
            cls <- use currentClause >>= errIfNothing (NoClause tableId)
            -- increment table num & retrieve the result
            curTblNum <- lastTable <+= 1
            let newCapt = prependNum curTblNum capt
            let tblInfo = Captioned curTblNum newCapt tableId cls
            currentRefs . tableRefs . at tableId .= Just tblInfo
            return newCapt

          stripSoftBreaks [] = []
          stripSoftBreaks (SoftBreak:xs) = Space:stripSoftBreaks xs
          stripSoftBreaks (x:xs) = x:stripSoftBreaks xs
          
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
    where registerNoteLike noteLikeType = do
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
                let newBlks = prependToBlocks [Cite [cite] []] tabSep blks
                return $ Div (divId, classes, kvals) newBlks
            where idPref = identifierPrefix noteLikeType
                  -- necessary to prevent the monomorphism restriction (?) from kicking in
                  inClause :: Lens' RefBuildingState [Identifier]
                  inClause = identifiersInClause noteLikeType
                  tabSep = [Space, ooxmlInline "<w:r><w:tab/></w:r>"]
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
            where newBlks = prependToBlocks [Str edNotePrefix] [Str ":", Space] blks


-- This is a hack: Pandoc uses "SourceCode" as the style for source code, while
-- the ISO template uses "Code".
processBlock (CodeBlock a content) = return $ Div ("", [], [("custom-style", "Code")]) [Para [Code a content]]

processBlock blk@(RawBlock (Format "tex") macro) 
    -- Make sure the next Header is treated as an annex start
    -- (this is a bit of a hack, but it should be fine as long as no 
    -- reference targets appear between \backmatter and the first
    -- header)
    | macro == "\\backmatter" = do
            currentClause .= Just (ClauseNum 0 [] True)
            return blk
    | otherwise = return blk  -- the other macros are processed elsewhere

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
    , ("example", "Example")
      -- TODO: implement auto-numbering for these
      -- (ISO uses the same Note template for notes to entry as well)
    , ("note-to-entry", "Note")
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


-- Pandoc's Docx writer inserts title/author/... into the actual document content, which
-- is undesirable. We rename the relevant meta entries here.
-- This unfortunately means that the associated docProps are also wrong, but that's something
-- we'll have to live with
rearrangeMeta :: [T.Text] -> Meta -> Meta
rearrangeMeta entries (Meta meta) = Meta $ foldr modEntry meta entries
    where modEntry k mp = case M.lookup k mp of
            Nothing -> mp
            -- let's not try to be clever with alterF to do the lookup/delete in one go
            Just v -> M.delete k (M.insert (k <> "-meta") v mp)


processMacrosInBlock :: Monad m => Block 
                     -> ReaderT Meta (ExceptT OOXMLMacroError m) Block

processMacrosInBlock blk@(RawBlock (Format "tex") macro) = fromMaybe blk <$> expandOOXMLMacro macro
processMacrosInBlock blk = return blk


processMacros :: Monad m => Pandoc -> ExceptT OOXMLMacroError m Pandoc
processMacros doc@(Pandoc meta _) = runReaderT (walkPandocM processMacrosInBlock doc) meta
