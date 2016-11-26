{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.Csv (FromNamedRecord, ToNamedRecord, DefaultOrdered, Header,
                (.:))
import qualified Data.Csv as Csv
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics

import Amby (Category)
import qualified Amby as Am

data Tip = Tip
  { _tTotalBill :: !Double
  , _tTip :: !Double
  , _tSex :: !Text
  , _tSmoker :: !Text
  , _tDay :: !Text
  , _tTime :: !Text
  , _tSize :: !Int
  } deriving (Show, Generic)
makeLenses ''Tip

data Tips = Tips
  { _totalBill :: V.Vector Double
  , _tip :: V.Vector Double
  , _sex :: Category
  , _smoker :: Category
  , _day :: Category
  , _time :: Category
  , _size :: V.Vector Int
  }
makeLenses ''Tips

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

parseCsv :: IO (Header, Tips)
parseCsv = do
  dat <- BL.readFile "./data/seaborn_tips.csv"
  case Csv.decodeByName dat of
    Left e -> error $ "Unable to parse csv file: " ++ e
    Right (headers, ds) -> return $ (headers, Tips
      { _totalBill = V.map _tTotalBill ds
      , _tip = V.map _tTip ds
      , _sex = Am.toCatOrdered
        (V.map _tSex ds)
        (V.fromList ["Male", "Female"])
      , _smoker = Am.toCatOrdered
        (V.map _tSmoker ds)
        (V.fromList ["Yes", "No"])
      , _day = Am.toCatOrdered
        (V.map _tDay ds)
        (V.fromList ["Thur", "Fri", "Sat", "Sun"])
      , _time = Am.toCatOrdered
        (V.map _tTime ds)
        (V.fromList ["Lunch", "Dinner"])
      , _size = V.map _tSize ds
      })

main :: IO ()
main = void parseCsv
