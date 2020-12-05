module Tryhard.Stats.Combo where

data Combo = Combo
  { comboSameTeam :: Int
  , comboOponentTeam :: Int
  } deriving (Eq)

with :: Combo
with = Combo 1 0

against :: Combo
against = Combo 0 1

instance Semigroup Combo where
  Combo { comboSameTeam = sameA, comboOponentTeam = enemyA } <> Combo { comboSameTeam = sameB, comboOponentTeam = enemyB }
    = Combo { comboSameTeam    = sameA + sameB
            , comboOponentTeam = enemyA + enemyB
            }

instance Ord Combo where
  compare Combo { comboSameTeam = sameA, comboOponentTeam = enemyA } Combo { comboSameTeam = sameB, comboOponentTeam = enemyB }
    = compare (sameA + enemyA) (sameB + enemyB)

instance Show Combo where
  show (Combo { comboSameTeam = same, comboOponentTeam = enemy }) =
    (show same) <> " with and " <> (show enemy) <> " against"
