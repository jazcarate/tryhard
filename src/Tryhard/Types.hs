module Tryhard.Types where

import           Data.Text                      ( Text
                                                , unpack
                                                )

newtype HeroID = HeroID { unHero :: Int }

data Hero = Hero {
  heroName :: Text,
  heroID :: HeroID
}

instance Show Hero where
  show = unpack . heroName
