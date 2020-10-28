{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tryhard.Types where

import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Data.Hashable                  ( Hashable
                                                , hashWithSalt
                                                )
import           Data.Function                  ( on )
import           Data.Aeson                     ( FromJSONKey )

newtype HeroID = HeroID { unHero :: Int } deriving (Eq, Ord, Hashable, FromJSONKey)

data Hero = Hero
  { heroID   :: HeroID
  , heroName :: Text
  , heroLegs :: Int
  } deriving (Eq)


instance Hashable Hero where
  hashWithSalt salt = hashWithSalt salt . heroID

instance Show Hero where
  show = unpack . heroName

data Matchup = Matchup
  { matchupHero      :: Hero
  , matchupGamesPlayed :: Int
  , matchupWins        :: Int
  }

instance Eq Matchup where
  (==) = (==) `on` matchupHero

class WithHero a where
  getHero :: a -> Hero

class (Monad m) => Stats container m res where
  for :: container -> Hero -> m [res]
