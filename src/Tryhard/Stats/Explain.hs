module Tryhard.Stats.Explain where

import qualified Data.Text                     as T

data Explain a = Explain
    { explanation :: T.Text
    , unExplain :: a
    }

instance Semigroup a => Semigroup (Explain a) where
  Explain { explanation = expA, unExplain = a } <> Explain { explanation = expB, unExplain = b }
    = Explain { explanation = expA <> " > " <> expB, unExplain = a <> b }
