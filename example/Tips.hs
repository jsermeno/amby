{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics

data Tip = Tip
  { _totalBill :: !Double
  , _tip :: !Double
  , _sex :: !Text
  , _smoker :: !Text
  , _day :: !Text
  , _time :: !Text
  , _size :: !Int
  } deriving (Show, Generic)
makeLenses ''Tip

instance FromNamedRecord Tip where
  parseNamedRecord m = Tip
    <$> m .: "total_bill"
    <*> m .: "tip"
    <*> m .: "sex"
    <*> m .: "smoker"
    <*> m .: "day"
    <*> m .: "time"
    <*> m .: "size"
instance ToNamedRecord Tip
instance DefaultOrdered Tip

parseCsv :: IO (Header, Vector Tip)
parseCsv = do
  dat <- BL.readFile "./data/seaborn_tips.csv"
  case decodeByName dat of
    Left e -> error $ "Unable to parse csv file: " ++ e
    Right a -> return a

main :: IO ()
main = void parseCsv
