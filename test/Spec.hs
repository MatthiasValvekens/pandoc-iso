import Test.HUnit
import Test.ASN1.Core
import Test.ASN1.UniversalTypes

main :: IO ()
main = do
    cts <- runTestTT $ test [coreParsingTests, universalParsingTests]
    putStrLn $ show cts
