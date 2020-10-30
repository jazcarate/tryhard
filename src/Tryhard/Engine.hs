module Tryhard.Engine where


import qualified Data.HashMap.Strict           as HM
import           Data.List                      ( sortBy )
import           Data.Function                  ( on )

import           Tryhard.Types


data Result = Result {
    resultHero :: Hero,
    resultValue :: String
}

instance Show Result where
  show Result { resultHero = hero, resultValue = value } =
    show hero <> " - " <> value

fromTuple :: Show a => (Hero, a) -> Result
fromTuple (h, a) = Result h (show a)

recomend :: (Ord res, Show res) => StatsResult res -> [Result]
recomend sr = reverse $ fromTuple <$> x sr
 where
  x :: Ord a => StatsResult a -> [(Hero, a)] -- TODO understand why if I inline this I get an ambigous type
  x sr' = sortBy (compare `on` snd) $ HM.toList $ unStatsResults $ sr'
