module Tryhard.Types where

import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Data.Hashable

newtype HeroID = HeroID { unHero :: Int } deriving (Eq)

instance Hashable HeroID where
  hashWithSalt salt = hashWithSalt salt . unHero

data Hero = Hero {
  heroName :: Text,
  heroID :: HeroID
} deriving (Eq)

instance Show Hero where
  show = unpack . heroName


data Matchup = Matchup {
  matchupHeroId :: HeroID,
  matchupGamesPlayed    :: Int,
  matchupWins ::        Int
}
