{-# LANGUAGE OverloadedStrings #-}

import System.IO (hPutStrLn, stderr)
import qualified Data.Text as T
import Text.Pandoc.ISO.Filter
import Text.Pandoc.JSON
import Control.Monad.Except


handleRefsWithErrors :: Pandoc -> IO Pandoc
handleRefsWithErrors doc = do
    refHandlingResult <- runExceptT (handleInternalRefs doc)
    case refHandlingResult of
        Left err -> errh err
        Right (newDoc, undefinedRefs) -> do
            mapM_ printWarning undefinedRefs
            handleStyles (fixMeta newDoc)
    where printWarning undefRef = hPutStrLn stderr warningStr 
            where warningStr = "[Warning] Undefined reference: " ++ T.unpack undefRef
          errh err = hPutStrLn stderr errStr >> return doc
            -- TODO improve messages
            where errStr = "[Error] Reference detection failure: " ++ show err
          fixMeta (Pandoc meta blks) 
            = Pandoc (rearrangeMeta ["author", "title", "date"] meta) blks

main :: IO ()
main = toJSONFilter handleRefsWithErrors
