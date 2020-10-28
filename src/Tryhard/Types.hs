{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tryhard.Types where

import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Data.Hashable                  ( Hashable
                                                , hashWithSalt
                                                )
import           Data.Function                  ( on )
import           Data.Aeson                     ( FromJSONKey )
import qualified Data.HashMap.Strict           as HM
import           Data.Tuple                     ( swap )

newtype HeroID = HeroID { unHero :: Int } deriving (Eq, Ord, Hashable, FromJSONKey)

data Hero = Hero
  { heroID   :: HeroID
  , heroName :: Text
  , heroLegs :: Int
  } deriving (Eq)


instance Hashable Hero where
  hashWithSalt salt = hashWithSalt salt . heroID

instance Show Hero where
  show = unpack . heroName

-- The entries are swapped for ease of `Compose`
statsFromList :: [(a, Hero)] -> StatsResult a
statsFromList entries = StatsResult $ HM.fromList $ swap <$> entries

data Matchup = Matchup
  { matchupGamesPlayed :: Int
  , matchupWins        :: Int
  }

newtype StatsResult a = StatsResult { unStatsResults :: HM.HashMap Hero a }

instance Semigroup a => Semigroup (StatsResult a) where
  (StatsResult a) <> (StatsResult b) = StatsResult $ HM.unionWith (<>) a b

empty :: StatsResult a
empty = StatsResult HM.empty

instance Functor StatsResult where
  fmap f = StatsResult . fmap f . unStatsResults

instance (Eq a) => Eq (StatsResult a) where
  (==) = (==) `on` unStatsResults

class (Monad m) => Stats container m res where
  for :: container -> Hero -> m (StatsResult res)

