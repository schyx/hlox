import Test.HUnit

import ScannerTests

main :: IO ()
main = do
  _ <- runTestTT scannerTests
  return ()
