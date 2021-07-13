import System.IO (hPutStrLn, stderr)
import qualified Data.Text as T
import Text.Pandoc.ISO
import Text.Pandoc.JSON
import Control.Monad.Except


handleRefsWithErrors :: Pandoc -> IO Pandoc
handleRefsWithErrors doc = do
    refHandlingResult <- runExceptT (handleInternalRefs doc)
    case refHandlingResult of
        Left err -> errh err
        Right (newDoc, undefinedRefs) -> do
            mapM_ printWarning undefinedRefs
            handleStyles newDoc
    where printWarning undefRef = hPutStrLn stderr warningStr 
            where warningStr = "[Warning] Undefined reference: " ++ T.unpack undefRef
          errh err = hPutStrLn stderr errStr >> return doc
            -- TODO improve messages
            where errStr = "[Error] Reference detection failure: " ++ show err

main :: IO ()
main = toJSONFilter handleRefsWithErrors
