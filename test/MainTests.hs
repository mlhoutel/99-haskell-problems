import Test.Tasty (TestTree, defaultMain, testGroup)

import Tests_01_10

main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Haskell 99 problems tests"
    [ suite_01_10 ]