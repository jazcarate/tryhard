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
import qualified Data.Set                      as S
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

instance Ord Hero where
  compare = compare `on` heroName

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
  forHero :: container -> Hero -> m (StatsResult res)

newtype TeamComp = TeamComp { unTeamComp :: S.Set Hero }

heroTC :: Hero -> TeamComp
heroTC = TeamComp . S.singleton

instance Semigroup TeamComp where
  (TeamComp a) <> (TeamComp b) = TeamComp $ a <> b

instance Monoid TeamComp where
  mempty = TeamComp $ mempty

instance Show TeamComp where
  show (TeamComp set) = show set

-- Could this be a Funcotr (monoid? maybe) and keep this structure to be mapped by stat results?
data LookAt = All MatchComp | MyTeam MatchComp | EnemyTeam MatchComp

toList :: LookAt -> [Hero]
toList la = case la of
  All       (MatchComp a b) -> toListComp a <> toListComp b
  MyTeam    (MatchComp a _) -> toListComp a
  EnemyTeam (MatchComp _ b) -> toListComp b
 where
  toListComp :: TeamComp -> [Hero]
  toListComp (TeamComp s) = S.toList s

data MatchComp = MatchComp { unMatchCompA :: TeamComp, unMatchCompB :: TeamComp }

instance Show MatchComp where
  show MatchComp { unMatchCompA = a, unMatchCompB = b } =
    show a <> " vs " <> show b

comp :: MatchComp
comp = MatchComp mempty mempty

-- TODO Make this lens
with :: TeamComp -> MatchComp -> MatchComp
with w MatchComp { unMatchCompA = a, unMatchCompB = b } =
  MatchComp { unMatchCompA = a <> w, unMatchCompB = b }

against :: TeamComp -> MatchComp -> MatchComp
against w MatchComp { unMatchCompA = a, unMatchCompB = b } =
  MatchComp { unMatchCompA = a, unMatchCompB = b <> w }
