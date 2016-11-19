import Test.DocTest

main :: IO ()
main = doctest
  [ "src/Amby/Numeric.hs"
  ]
