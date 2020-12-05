module Tryhard.Stats.NumberOfLegs where

import           Data.Function                  ( on )

import           Tryhard.Stats.Types
import           Tryhard.Hero

newtype NumberOfLegs = NumberOfLegs { legDifference :: Int } deriving (Eq)

numberOfLegs :: Hero -> Hero -> NumberOfLegs
numberOfLegs a b = NumberOfLegs (abs (heroLegs a - heroLegs b))

instance Show NumberOfLegs where
  show = (\s -> "Δ " ++ s ++ " legs") . (show . legDifference)

instance Ord NumberOfLegs where
  compare = (notOrd .) . (compare `on` (legDifference))

notOrd :: Ordering -> Ordering
notOrd a = case a of
  EQ -> EQ
  LT -> GT
  GT -> LT

instance Invertable NumberOfLegs where
  invert = id

instance Summable NumberOfLegs where
  NumberOfLegs { legDifference = a } <+> NumberOfLegs { legDifference = b } =
    NumberOfLegs { legDifference = a + b }
