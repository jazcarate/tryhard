module Tryhard.Stats.Types where

data ShouldInvert a = DontInvert a | Invert a

instance Functor ShouldInvert where
  fmap f si = case si of
    DontInvert a -> DontInvert (f a)
    Invert     a -> Invert (f a)

extract :: (Invertable a) => ShouldInvert a -> a
extract si = case si of
  DontInvert a -> a
  Invert     a -> invert a

class Invertable a where
  invert :: a -> a

class Summable a where
    (<+>) :: a -> a -> a
