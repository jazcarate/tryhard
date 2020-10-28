module Test.Fixture where

import           Data.Text                      ( Text )
import qualified Data.HashMap.Strict           as HM

import           Tryhard.Types
import           Tryhard.Stats

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

vsAxe :: Matchup
vsAxe = matchup axe 10 5

vsBane :: Matchup
vsBane = matchup bane 100 0

matchups :: ConstMathcupMap
matchups = newConstMatchupMatrix $ HM.fromList [(antiMage, [vsAxe, vsBane])]
