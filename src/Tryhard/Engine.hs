module Tryhard.Engine where

import           Tryhard.Types

recomend :: (Stats map m res) => map -> Hero -> m [res]
recomend matrix hero = matrix `for` hero
