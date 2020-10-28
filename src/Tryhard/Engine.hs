module Tryhard.Engine where


import qualified Data.HashMap.Strict           as HM
import           Data.List                      ( sort )
import           Data.Function                  ( on )

import           Tryhard.Types


data Result a = Result {
    resultHero :: Hero,
    resultValue :: a
} deriving (Show)

fromTuple :: (Hero, a) -> Result a
fromTuple = uncurry Result

instance (Eq a) => Eq (Result a) where
  (==) = (==) `on` resultValue

instance (Ord a) => Ord (Result a) where
  compare = compare `on` resultValue


recomend :: (Ord res) => StatsResult res -> [Result res]
recomend sr = reverse $ sort $ fromTuple <$> x sr
 where
  x :: StatsResult a -> [(Hero, a)] -- TODO understand why if I inline this I get an ambigous type
  x sr' = HM.toList $ unStatsResults sr'
