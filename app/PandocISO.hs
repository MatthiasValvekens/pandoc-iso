{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Except
import qualified Data.Text as T
import System.IO (hPutStrLn, stderr)
import Text.Pandoc.ISO.Filter
import Text.Pandoc.JSON

handleRefsWithErrors :: Pandoc -> IO Pandoc
handleRefsWithErrors doc = do
  withRefs <- runWithErrAndWarningH refErrh (mapM_ refWarning) handleInternalRefs doc
  withTablesFormatted <- runWithWarningH (mapM_ tableWarning) fixTableFormatting withRefs
  withMacrosApplied <- runWithErrH macroErrh processMacros withTablesFormatted
  handleStyles (fixMeta withMacrosApplied)
  where
    tableWarning (Unsimplifiable tbl) = hPutStrLn stderr warningStr
      where
        warningStr = "[Warning] Table tbl was too complex to be set in ISO template " ++ show tbl
    tableWarning (MacroError err) = hPutStrLn stderr warningStr
      where
        warningStr = "[Warning] Templating error while formatting table: " ++ show err
    refWarning undefRef = hPutStrLn stderr warningStr
      where
        warningStr = "[Warning] Undefined reference: " ++ T.unpack undefRef
    refErrh err = hPutStrLn stderr errStr
      where
        -- TODO improve messages
        errStr = "[Error] Reference detection failure: " ++ show err
    macroErrh err = hPutStrLn stderr errStr
      where
        -- TODO improve messages
        errStr = "[Error] Macro expansion failure: " ++ show err
    fixMeta (Pandoc meta blks) =
      Pandoc (rearrangeMeta ["author", "title", "date"] meta) blks


runWithErrH' ::
  Monad m =>
  (e -> m ()) ->
  (Pandoc -> ExceptT e m a) ->
  (a -> m Pandoc) ->
  Pandoc ->
  m Pandoc
runWithErrH' errh action postAction theDoc = do
  actionResult <- runExceptT (action theDoc)
  case actionResult of
    Left err -> errh err >> return theDoc
    Right result -> postAction result


runWithErrH ::
  Monad m =>
  (e -> m ()) ->
  (Pandoc -> ExceptT e m Pandoc) ->
  Pandoc ->
  m Pandoc
runWithErrH errh action = runWithErrH' errh action return


runWithErrAndWarningH ::
  Monad m =>
  (e -> m ()) ->
  (w -> m ()) ->
  (Pandoc -> ExceptT e m (Pandoc, w)) ->
  Pandoc ->
  m Pandoc
runWithErrAndWarningH errh warnh action = runWithErrH' errh action warnh'
  where warnh' (newDoc, w) = warnh w >> return newDoc


runWithWarningH ::
  Monad m =>
  (w -> m ()) ->
  (Pandoc -> m (Pandoc, w)) ->
  Pandoc ->
  m Pandoc
runWithWarningH warnh action doc = do
    (newDoc, w) <- action doc
    warnh w
    return newDoc


main :: IO ()
main = toJSONFilter handleRefsWithErrors
