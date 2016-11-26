{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
module Amby.Util
  ( loadDataset
  , getCol

  -- * Datasets
  , Tip(..)
  , Iris(..)
  , tips
  , getTipColumns
  , iris
  , getIrisColumns

  -- * Tip lenses
  , totalBill
  , tip
  , sex
  , day
  , smoker
  , time
  , tipSize

  -- * Iris lenses
  , sepalLength
  , sepalWidth
  , petalLength
  , petalWidth
  , irisClass
  ) where

import Control.Lens
import qualified Data.ByteString.Lazy as BL
import Data.Csv ((.:))
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import GHC.Generics
import Paths_amby (getDataFileName)

import Numeric.Datasets (Dataset, getDataset)
import qualified Numeric.Datasets.Iris as Iris

import Amby.Categorical

loadDataset :: Dataset a -> IO [a]
loadDataset ds = do
  getDataset ds

data Tip = Tip
  { _tipTotalBill :: !Double
  , _tipTip :: !Double
  , _tipSex :: !String
  , _tipSmoker :: !String
  , _tipDay :: !String
  , _tipTime :: !String
  , _tipTipSize :: !Int
  } deriving (Generic)
makeFields ''Tip

instance Show Tip where
  show t =
      showString "Tip "
    $ showString "{totalBill = "
    $ showString (show (_tipTotalBill t))
    $ showString ", tip = "
    $ showString (show (_tipTip t))
    $ showString ", sex = "
    $ showString (show (_tipSex t))
    $ showString ", smoker = "
    $ showString (show (_tipSmoker t))
    $ showString ", day = "
    $ showString (show (_tipDay t))
    $ showString ", time = "
    $ showString (show (_tipTime t))
    $ showString ", tipSize = "
    $ showString (show (_tipTipSize t)) "}"

data TipColumns = TipColumns
  { _tipColumnsTotalBill :: [Double]
  , _tipColumnsTip :: [Double]
  , _tipColumnsSex :: Category
  , _tipColumnsSmoker :: Category
  , _tipColumnsDay :: Category
  , _tipColumnsTime :: Category
  , _tipColumnsTipSize :: [Int]
  }
makeFields ''TipColumns

data Iris = Iris
  { _irisSepalLength :: Double
  , _irisSepalWidth :: Double
  , _irisPetalLength :: Double
  , _irisPetalWidth :: Double
  , _irisIrisClass :: Iris.IrisClass
  } deriving (Show)
makeFields ''Iris

data IrisColumns = IrisColumns
  { _irisColumnsSepalLength :: [Double]
  , _irisColumnsSepalWidth :: [Double]
  , _irisColumnsPetalLength :: [Double]
  , _irisColumnsPetalWidth :: [Double]
  , _irisColumnsIrisClass :: Category
  }
makeFields ''IrisColumns

class Loader a where
  type RowValue a :: *
  getCol ::  Lens (RowValue a) (RowValue a) b b -> a -> b

instance Loader [Tip] where
  type RowValue [Tip] = TipColumns
  getCol gtr ds = TipColumns
    { _tipColumnsTotalBill = map (^. totalBill) ds
    , _tipColumnsTip = map (^. tip) ds
    , _tipColumnsSex = toCatOrdered (map (^. sex) ds) ["Male", "Female"]
    , _tipColumnsSmoker = toCatOrdered (map (^. smoker) ds) ["Yes", "No"]
    , _tipColumnsDay = toCatOrdered (map (^. day) ds) ["Thur", "Fri", "Sat", "Sun"]
    , _tipColumnsTime = toCatOrdered (map (^. time) ds) ["Lunch", "Dinner"]
    , _tipColumnsTipSize = map (^. tipSize) ds
    } ^. gtr

instance Loader [Iris] where
  type RowValue [Iris] = IrisColumns
  getCol gtr ds = IrisColumns
    { _irisColumnsSepalLength = map (^. sepalLength) ds
    , _irisColumnsSepalWidth = map (^. sepalWidth) ds
    , _irisColumnsPetalLength = map (^. petalLength) ds
    , _irisColumnsPetalWidth = map (^. petalWidth) ds
    , _irisColumnsIrisClass = toCat (map (^. irisClass) ds)
    } ^. gtr

------------------------
-- Tips dataset
------------------------

instance Csv.FromNamedRecord Tip where
  parseNamedRecord m = Tip
    <$> m .: "total_bill"
    <*> m .: "tip"
    <*> m .: "sex"
    <*> m .: "smoker"
    <*> m .: "day"
    <*> m .: "time"
    <*> m .: "size"
instance Csv.ToNamedRecord Tip
instance Csv.DefaultOrdered Tip

tips :: Dataset Tip
tips _ = do
  csvPath <- getDataFileName "data/seaborn_tips.csv"
  dat <- BL.readFile csvPath
  case Csv.decodeByName dat of
    Left e -> error $ "Unable to parse csv file: " ++ e
    Right (_hs, ds) -> return $ V.toList ds

getTipColumns :: [Tip]
              -> IO ( [Double]
                    , [Double]
                    , Category
                    , Category
                    , Category
                    , Category
                    , [Int]
                    )
getTipColumns ts = return
  ( getCol totalBill ts
  , getCol tip ts
  , getCol sex ts
  , getCol smoker ts
  , getCol day ts
  , getCol time ts
  , getCol tipSize ts
  )

iris :: Dataset Iris
iris f = Iris.iris f >>= mapM dsIrisToIris

dsIrisToIris :: Iris.Iris -> IO Iris
dsIrisToIris oIris = return $ Iris
  { _irisSepalLength = Iris.sepalLength oIris
  , _irisSepalWidth = Iris.sepalWidth oIris
  , _irisPetalLength = Iris.petalLength oIris
  , _irisPetalWidth = Iris.petalWidth oIris
  , _irisIrisClass = Iris.irisClass oIris
  }

getIrisColumns :: [Iris]
               -> IO ( [Double]
                     , [Double]
                     , [Double]
                     , [Double]
                     , Category
                     )
getIrisColumns is = return
  ( getCol sepalLength is
  , getCol sepalWidth is
  , getCol petalLength is
  , getCol petalWidth is
  , getCol irisClass is
  )
