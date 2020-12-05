{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tryhard.Hero where

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

instance Ord Hero where
  compare = compare `on` heroName
