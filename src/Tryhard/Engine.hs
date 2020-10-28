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

-- TODO if I inline `recomend'`I get a type error.. why?
recomend' :: StatsResult res -> [(Hero, res)]
recomend' sr = HM.toList $ unStatsResults sr


recomend :: (Ord res) => StatsResult res -> [Result res]
recomend sr = reverse $ sort $ fromTuple <$> recomend' sr
