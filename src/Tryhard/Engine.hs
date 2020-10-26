module Tryhard.Engine where

import           Tryhard.Types
import           Tryhard.OpenDota
import           Data.Maybe                     ( catMaybes )
import           Data.List                      ( find )

recomend :: (MatchupMap map m) => [Hero] -> map -> Hero -> m [Hero]
recomend db matrix hero = do
  matchups <- matrix `for` (heroID hero)
  let heroes = bindHeros db <$> matchupHeroId <$> matchups
  pure $ catMaybes heroes

bindHeros :: [Hero] -> HeroID -> Maybe Hero
bindHeros db id' = find (\hero -> heroID hero == id') db
