{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

-- This is very strange, but as I'm unioning Stats reults based on heros; this lets me `mconcat` over statresults of heroes
instance Semigroup Hero where
  h <> _ = h

-- The entries are swapped for ease of `Compose`
statsFromList :: [(a, Hero)] -> StatsResult a
statsFromList entries = StatsResult $ HM.fromList $ swap <$> entries

data Matchup = Matchup
  { matchupGamesPlayed :: Int
  , matchupWins        :: Int
  }

instance Semigroup Matchup where
  (Matchup { matchupGamesPlayed = played1, matchupWins = wins1 }) <> (Matchup { matchupGamesPlayed = played2, matchupWins = wins2 })
    = Matchup { matchupGamesPlayed = played1 + played2
              , matchupWins        = wins1 + wins2
              }

newtype StatsResult a = StatsResult { unStatsResults :: HM.HashMap Hero a }

instance (Semigroup a) => Semigroup (StatsResult a) where
  (StatsResult a) <> (StatsResult b) = StatsResult $ HM.unionWith (<>) a b

instance (Semigroup a) => Monoid (StatsResult a) where
  mempty = StatsResult HM.empty

empty :: StatsResult a
empty = StatsResult HM.empty

instance Functor StatsResult where
  fmap f = StatsResult . fmap f . unStatsResults

instance (Eq a) => Eq (StatsResult a) where
  (==) = (==) `on` unStatsResults

newtype TeamComp = TeamComp { unTeamComp :: S.Set Hero }

heroTC :: Hero -> TeamComp
heroTC = TeamComp . S.singleton

instance Semigroup TeamComp where
  (TeamComp a) <> (TeamComp b) = TeamComp $ a <> b

instance Monoid TeamComp where
  mempty = TeamComp $ mempty

instance Show TeamComp where
  show (TeamComp set) = show set

data MatchComp = MatchComp { unMatchCompA :: TeamComp, unMatchCompB :: TeamComp }

newtype MyTeam = MyTeam { unMyTeam :: MatchComp }
newtype EnemyTeam = EnemyTeam { unEnemyTeam :: MatchComp }

instance Listable MyTeam where
  toList (MyTeam (MatchComp a _)) = toList a

instance Listable EnemyTeam where
  toList (EnemyTeam (MatchComp _ b)) = toList b

instance Listable TeamComp where
  toList (TeamComp s) = S.toList s

instance Listable MatchComp where
  toList (MatchComp a b) = toList a <> toList b

class Listable a where
  toList :: a -> [Hero]

instance Show MatchComp where
  show MatchComp { unMatchCompA = a, unMatchCompB = b } =
    show a <> " vs " <> show b

comp :: MatchComp
comp = MatchComp mempty mempty

-- TODO Make lens out of team comp
with :: TeamComp -> MatchComp -> MatchComp
with w MatchComp { unMatchCompA = a, unMatchCompB = b } =
  MatchComp { unMatchCompA = a <> w, unMatchCompB = b }

against :: TeamComp -> MatchComp -> MatchComp
against w MatchComp { unMatchCompA = a, unMatchCompB = b } =
  MatchComp { unMatchCompA = a, unMatchCompB = b <> w }
