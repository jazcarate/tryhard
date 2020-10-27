module Tryhard.Fixture where

import           Tryhard.Types
import           Data.Text                      ( Text )
import           Tryhard.OpenDota
import qualified Data.HashMap.Strict           as HM

hero :: Text -> Int -> Hero
hero name id' = Hero { heroName = name, heroID = HeroID id' }

antiMage :: Hero
antiMage = hero "Anti-Mage" 1

axe :: Hero
axe = hero "Axe" 2

bane :: Hero
bane = hero "Bane" 3

matchup :: Hero -> Int -> Int -> Matchup
matchup hero' played wins = Matchup { matchupHero        = hero'
                                    , matchupGamesPlayed = played
                                    , matchupWins        = wins
                                    }

matchups :: ConstMathcupMap
matchups = newConstMatchupMatrix
  $ HM.fromList [(antiMage, [matchup axe 10 5, matchup bane 100 0])]
