module Tryhard.Picks where

import qualified Data.Set                      as S
import           Data.Bifunctor                 ( bimap )
import           Data.Hashable                  ( Hashable
                                                , hashWithSalt
                                                )

import           Tryhard.Hero

data Alliance = Friendly | Enemy deriving (Eq, Ord)
data TeamHero = TeamHero
    { unTeamHero :: Hero
    , unTeamAlliance :: Alliance
    } deriving (Eq, Ord)

newtype Picks = Picks { unPicks :: S.Set TeamHero } deriving (Eq)

pick :: TeamHero -> Picks
pick = Picks . S.singleton

friendly :: Hero -> Picks
friendly h = pick $ TeamHero h Friendly

isFriendly :: TeamHero -> Bool
isFriendly = (== Friendly) . unTeamAlliance


enemy :: Hero -> Picks
enemy h = pick $ TeamHero h Enemy

instance Semigroup Picks where
  Picks a <> Picks b = Picks $ a <> b

instance Monoid Picks where
  mempty = Picks mempty


instance Hashable TeamHero where
  hashWithSalt salt = hashWithSalt salt . unTeamHero -- TODO : Hash both team hero and alliance

instance Hashable Picks where
  hashWithSalt salt = hashWithSalt salt . S.toList . unPicks

elem :: TeamHero -> Picks -> Bool
elem t = S.member t . unPicks

-- | (Allies, Enemies)
teamsS :: Picks -> (S.Set Hero, S.Set Hero)
teamsS = bimap toHero toHero . S.partition isFriendly . unPicks
  where toHero = S.map unTeamHero

teams :: Picks -> ([Hero], [Hero])
teams = bimap S.toList S.toList . teamsS

teamsList :: Picks -> [Hero]
teamsList p = S.toList $ a <> b where (a, b) = teamsS p
