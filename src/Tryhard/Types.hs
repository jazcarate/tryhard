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
import           Control.Monad                  ( liftM2 )

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

data Matchup = Matchup
  { matchupHero      :: Hero
  , matchupGamesPlayed :: Int
  , matchupWins        :: Int
  }

instance Eq Matchup where
  (==) = (==) `on` matchupHero

class WithHero a where
  getHero :: a -> Hero

class Valuable a where
  value :: Fractional p => a -> p


data InnerResult a
  = InnerResult { unInnerResultHero :: Hero, unInnerResult :: a }

instance Functor InnerResult where
  fmap f InnerResult { unInnerResultHero = hero, unInnerResult = a } =
    InnerResult hero (f a)

instance (Eq a) => Eq (InnerResult a) where
  (==) = ((==) `on` unInnerResult) &&& ((==) `on` unInnerResultHero)
   where
    (&&&) :: (Eq b) => (b -> b -> Bool) -> (b -> b -> Bool) -> b -> b -> Bool
    (&&&) = liftM2 (liftM2 (&&))

instance (Ord a) => Ord (InnerResult a) where
  compare = compare `on` unInnerResult

instance WithHero a => WithHero (InnerResult a) where
  getHero = getHero . unInnerResult

newtype StatsResult a = StatsResult { unStatsResults :: [InnerResult a] }

instance Semigroup (StatsResult a) where
  a <> b = StatsResult $ ((<>) `on` unStatsResults) a b

instance Monoid (StatsResult a) where
  mempty = StatsResult mempty

instance Functor StatsResult where
  fmap f sr = StatsResult sr'
    where sr' = (\x -> f <$> x) <$> (unStatsResults sr)

instance (Eq a) => Eq (StatsResult a) where
  (==) = (==) `on` unStatsResults

instance (Ord a) => Ord (StatsResult a) where
  compare = compare `on` unStatsResults

statsResult :: Hero -> [a] -> StatsResult a
statsResult h as = StatsResult $ InnerResult h <$> as

toList :: StatsResult a -> [InnerResult a]
toList = unStatsResults

class (Monad m) => Stats container m res where
  for :: container -> Hero -> m (StatsResult res)

