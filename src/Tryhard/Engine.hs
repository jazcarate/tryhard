module Tryhard.Engine where

import           Tryhard.Types
import           Tryhard.OpenDota
import           Data.List                      ( find )

recomend :: (MatchupMap map m res) => map -> Hero -> m [res]
recomend matrix hero = matrix `for` (heroID hero)

bindHeros :: [Hero] -> HeroID -> Maybe Hero
bindHeros db id' = find (\hero -> heroID hero == id') db
