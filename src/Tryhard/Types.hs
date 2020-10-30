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

newtype LHS a = LHS { unLHS :: a }

-- Warning: This semigroup is not well behaved. Please use with caution
instance Semigroup (LHS a) where
  a <> _ = a

instance Hashable Hero where
  hashWithSalt salt = hashWithSalt salt . heroID

instance Show Hero where
  show = unpack . heroName

instance Ord Hero where
  compare = compare `on` heroName

-- The entries are swapped for ease of `Compose`
statsFromList :: (Semigroup a) => [(a, Hero)] -> StatsResult a
statsFromList entries = StatsResult $ HM.fromListWith (<>) $ swap <$> entries

data Matchup = Matchup
  { matchupGamesPlayed :: Int
  , matchupWins        :: Int
  }

data Combo = Combo
  { comboSameTeam :: Int
  , comboOponentTeam :: Int
  }

withC :: Combo
withC = Combo 1 0

againstC :: Combo
againstC = Combo 0 1

instance Semigroup Combo where
  Combo { comboSameTeam = sameA, comboOponentTeam = enemyA } <> Combo { comboSameTeam = sameB, comboOponentTeam = enemyB }
    = Combo { comboSameTeam    = sameA + sameB
            , comboOponentTeam = enemyA + enemyB
            }

newtype ByWith = ByWith { unByWith :: Combo }

instance Eq ByWith where
  (==) = (==) `on` (comboSameTeam . unByWith)

instance Ord ByWith where
  compare = compare `on` (comboSameTeam . unByWith)

instance Show ByWith where
  show (ByWith Combo { comboSameTeam = same, comboOponentTeam = enemy }) =
    (show same) <> " with and " <> (show enemy) <> " against"

newtype ByAgainst = ByAgainst { unByAgainst :: Combo }

instance Eq ByAgainst where
  (==) = (==) `on` (comboOponentTeam . unByAgainst)

instance Ord ByAgainst where
  compare = compare `on` (comboOponentTeam . unByAgainst)

instance Show ByAgainst where
  show (ByAgainst Combo { comboSameTeam = same, comboOponentTeam = enemy }) =
    (show enemy) <> " against and " <> (show same) <> " with"

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

newtype TeamComp = TeamComp { unTeamComp :: S.Set Hero } deriving (Eq)

instance Hashable TeamComp where
  hashWithSalt salt (TeamComp a) = hashWithSalt salt (S.toList a)

heroTC :: Hero -> TeamComp
heroTC = TeamComp . S.singleton

instance Semigroup TeamComp where
  (TeamComp a) <> (TeamComp b) = TeamComp $ a <> b

instance Monoid TeamComp where
  mempty = TeamComp $ mempty

instance Show TeamComp where
  show (TeamComp set) = show set

data MatchComp = MatchComp { unMatchCompA :: TeamComp, unMatchCompB :: TeamComp } deriving (Eq)

instance Hashable MatchComp where
  hashWithSalt salt (MatchComp a b) = salt `hashWithSalt` a `hashWithSalt` b

newtype MyTeam = MyTeam { unMyTeam :: MatchComp }
newtype EnemyTeam = EnemyTeam { unEnemyTeam :: MatchComp }

instance Listable MyTeam where
  toTuple (MyTeam (MatchComp a _)) = (toListComp a, mempty)

instance Listable EnemyTeam where
  toTuple (EnemyTeam (MatchComp _ b)) = (mempty, toListComp b)

toListComp :: TeamComp -> [Hero]
toListComp (TeamComp s) = S.toList s

instance Listable MatchComp where
  toTuple (MatchComp a b) = (toListComp a, toListComp b)

class Listable a where
  toTuple :: a -> ([Hero], [Hero])
  toList :: a -> [Hero]
  toList a = uncurry (<>) $ toTuple a

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
