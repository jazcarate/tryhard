module Tryhard.Heroes where

import           Data.Text                      ( Text
                                                )
type Heroes = [Hero]

newtype HeroID = HeroID { unHeroID :: Int } deriving (Show, Eq)

data Hero = Hero {
  heroName :: Text,
  heroID :: HeroID
} deriving (Show)

heroes :: IO Heroes
heroes = undefined