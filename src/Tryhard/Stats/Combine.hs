module Tryhard.Stats.Combine where

import           Data.Function                  ( on )

import           Tryhard.Stats.Types

newtype Sum a = Sum { getSum :: a } deriving (Eq, Ord)

instance Summable a => Semigroup (Sum a) where
  (Sum a) <> (Sum b) = Sum $ a <+> b

instance Show a => Show (Sum a) where
  show (Sum a) = "sum: " <> show a

newtype Max a = Max { getMax :: a } deriving (Eq, Ord)

instance Ord a => Semigroup (Max a) where
  a <> b = case (compare `on` getMax) a b of
    EQ -> a
    GT -> a
    LT -> b

instance Show a => Show (Max a) where
  show (Max a) = "max: " <> show a
