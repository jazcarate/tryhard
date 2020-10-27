-- TODO: Format this file!!

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tryhard.Types where

import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Data.Hashable
import           Data.Ratio
import           Data.Function                  ( on )
import           Numeric                        ( showGFloat )
import           Data.Aeson

newtype HeroID = HeroID { unHero :: Int } deriving (Eq, Ord, Hashable, FromJSONKey)

data Hero = Hero
  { heroID   :: HeroID
  , heroName :: Text
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

newtype WinPercentage = WinPercentage { unWinPercentageMatchup :: Matchup }

instance WithHero WinPercentage where
  getHero = matchupHero . unWinPercentageMatchup

instance Eq WinPercentage where
  (==) = (==) `on` unWinPercentageMatchup

instance Ord WinPercentage where
  compare = compare `on` (winRate . unWinPercentageMatchup)

winRate :: Matchup -> Ratio Int
winRate m = (matchupWins m) % (matchupGamesPlayed m)

instance Show WinPercentage where
  show = show . showRatio . ((* 100)) . winRate . unWinPercentageMatchup

showRatio :: Ratio Int -> String
showRatio r = showGFloat (Just 2) val ""
 where
  val :: Float
  val = (realToFrac $ numerator r) / (realToFrac $ denominator r)


newtype NumberOfMatches = NumberOfMatches { unNumberOfMatchesMatchup :: Matchup }

instance Eq NumberOfMatches where
  (==) = (==) `on` (matchupGamesPlayed . unNumberOfMatchesMatchup)

instance Ord NumberOfMatches where
  compare = compare `on` (matchupGamesPlayed . unNumberOfMatchesMatchup)

instance Show NumberOfMatches where
  show = (show . matchupGamesPlayed . unNumberOfMatchesMatchup)

instance WithHero NumberOfMatches where
  getHero = matchupHero . unNumberOfMatchesMatchup

class (Monad m) => Stats container m res where
  for :: container -> Hero -> m [res]
