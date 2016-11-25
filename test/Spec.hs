import qualified Data.Vector.Unboxed as U

import Test.DocTest
import Test.Tasty
import Test.Tasty.HUnit

import qualified Amby as Am

main :: IO ()
main = do
  doctest
    [ "src/Amby/Numeric.hs"
    , "src/Amby/Container.hs"
    , "src/Amby/Theme.hs"
    ]
  defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests"
  [ testCase "linspace vector generation" $
      Am.linspace 0 5 6 @?= U.fromList [0.0, 1.0, 2.0, 3.0, 4.0, 5.0]
  , testCase "arange vector generation" $
      Am.arange 0 5 1 @?= U.fromList [0.0, 1.0, 2.0, 3.0, 4.0, 5.0]
  ]
