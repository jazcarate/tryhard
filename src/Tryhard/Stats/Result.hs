module Tryhard.Stats.Result where

import           Data.Function                  ( on )
import qualified Data.HashMap.Strict           as HM
import           Data.Tuple                     ( swap )

import           Tryhard.Hero

fromList :: (Semigroup a) => [(a, Hero)] -> Result a
fromList entries = Result $ HM.fromListWith (<>) $ swap <$> entries

recomend :: (Ord a, Show a) => Result a -> [(Hero, String)]
recomend (Result a) = (\(h, v) -> (h, show v)) <$> HM.toList a

newtype Result a = Result
  { unResults :: HM.HashMap Hero a }

instance (Semigroup a) => Semigroup (Result a) where
  (Result a) <> (Result b) = Result $ HM.unionWith (<>) a b

instance (Semigroup a) => Monoid (Result a) where
  mempty = Result HM.empty

instance Functor Result where
  fmap f = Result . fmap f . unResults

instance (Eq a) => Eq (Result a) where
  (==) = (==) `on` unResults
