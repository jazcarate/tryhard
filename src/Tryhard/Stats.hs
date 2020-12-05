module Tryhard.Stats where

import           Data.Functor.Compose           ( getCompose
                                                , Compose(Compose)
                                                )

import           Tryhard.Stats.Result
import           Tryhard.Picks

-- TODO if Result is traversable, m can be "inside" a and sequence after to not keep track of `m`
newtype Stats m a = Stats { runStats :: Picks -> m (Result a) }

instance (Functor m) => Functor (Stats m) where
  fmap f (Stats r) = Stats $ \h -> getCompose $ f <$> Compose (r h)
