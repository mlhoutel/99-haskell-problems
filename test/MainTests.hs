import Test.Tasty (TestTree, defaultMain, testGroup)

import Tests_01_10
import Tests_11_20
import Tests_21_28

main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Haskell 99 problems tests"
    [ suite_01_10
      , suite_11_20
      , suite_21_28 ]