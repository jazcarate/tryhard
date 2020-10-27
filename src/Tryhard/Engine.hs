module Tryhard.Engine where

import           Tryhard.Types
import           Data.List                      ( find )

recomend :: (Stats map m res) => map -> Hero -> m [res]
recomend matrix hero = matrix `for` hero

-- TODO: change to a map
bindHeros :: [Hero] -> HeroID -> Maybe Hero
bindHeros db id' = find (\hero -> heroID hero == id') db
