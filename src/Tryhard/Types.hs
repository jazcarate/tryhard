module Tryhard.Types where

import           Data.Algebra.Free              ( FreeSemiGroup )
import           Data.Functor.Identity          ( runIdentity
                                                , Identity
                                                )


import           Tryhard.Stats
import           Tryhard.Stats.Types
import           Tryhard.Stats.Combo
import           Tryhard.Stats.Matchup
import           Tryhard.Stats.NumberOfLegs
import           Tryhard.OpenDota.HeroDB


newtype LHS a = LHS { unLHS :: a }

-- Warning: This semigroup is not well behaved. Please use with caution
instance Semigroup (LHS a) where
  a <> _ = a

-- TODO really need to clean up packages
data DataSources = DataSources
  { dataSourceHeroDB :: HeroDB
  , dataSourceMatchup :: Stats IO (FreeSemiGroup (ShouldInvert Matchup))
  , dataSourceNumberOfLegs :: Stats Identity (FreeSemiGroup NumberOfLegs)
  , dataSourceCombo :: Stats IO Combo
  }


class (Monad m) => ToIO m where
  lift :: m a -> IO a

instance ToIO IO where
  lift = id

instance ToIO Identity where
  lift = pure . runIdentity
