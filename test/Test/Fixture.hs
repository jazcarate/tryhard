module Test.Fixture where

import           Data.Text                      ( Text )
import qualified Data.HashMap.Strict           as HM

import           Tryhard.Types
import           Tryhard.Stats

hero :: Text -> Int -> Int -> Hero
hero name id' legs =
  Hero { heroName = name, heroID = HeroID id', heroLegs = legs }

antiMage :: Hero
antiMage = Hero { heroName = "Anti-Mage", heroID = HeroID 1, heroLegs = 2 }

axe :: Hero
axe = Hero { heroName = "Axe", heroID = HeroID 2, heroLegs = 2 }

bane :: Hero
bane = Hero { heroName = "Bane", heroID = HeroID 3, heroLegs = 4 }

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
