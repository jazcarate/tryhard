module Test.Fixture where

import           Data.Text                      ( Text )

import           Tryhard.Hero
import           Tryhard.Stats.Matchup

hero :: Text -> Int -> Int -> Hero
hero name id' legs =
  Hero { heroName = name, heroID = HeroID id', heroLegs = legs }

antiMage :: Hero
antiMage = Hero { heroName = "Anti-Mage", heroID = HeroID 1, heroLegs = 2 }

axe :: Hero
axe = Hero { heroName = "Axe", heroID = HeroID 2, heroLegs = 2 }

bane :: Hero
bane = Hero { heroName = "Bane", heroID = HeroID 3, heroLegs = 4 }

matchup :: Int -> Int -> Matchup
matchup played wins =
  Matchup { matchupGamesPlayed = played, matchupWins = wins }


matches :: Int -> Matchup
matches n = matchup n 30

percent :: Int -> Matchup
percent n = matchup 100 n

notPlayed :: Matchup
notPlayed = matchup 0 0
