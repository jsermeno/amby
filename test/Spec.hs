import Test.DocTest
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  doctest
    [ "src/Amby/Numeric.hs"
    ]
  defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests"
  [
  ]
