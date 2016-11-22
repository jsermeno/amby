import qualified Statistics.Distribution.Normal as Stats
import qualified Amby as Am

main :: IO ()
main = do
  z <- Am.random Stats.standard 10000
  Am.save $ Am.rugPlot' z
