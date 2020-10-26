module Tryhard.Types where

import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Data.Hashable
import           Data.Ratio
import           Data.Function                  ( on )
import           Numeric                        ( showGFloat )

newtype HeroID = HeroID { unHero :: Int } deriving (Eq)

instance Hashable HeroID where
  hashWithSalt salt = hashWithSalt salt . unHero

data Hero = Hero
  { heroName :: Text
  , heroID   :: HeroID
  } deriving (Eq)

instance Show Hero where
  show = unpack . heroName


data Matchup = Matchup
  { matchupHeroId      :: HeroID
  , matchupGamesPlayed :: Int
  , matchupWins        :: Int
  }


instance Eq Matchup where
  (==) = (==) `on` matchupHeroId

class WithHero a where
  getHero :: a -> HeroID

newtype WinPercentage = WinPercentage { unWinPercentageMatchup :: Matchup }

instance WithHero WinPercentage where
  getHero = matchupHeroId . unWinPercentageMatchup

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

instance WithHero NumberOfMatches where
  getHero = matchupHeroId . unNumberOfMatchesMatchup

instance Eq NumberOfMatches where
  (==) = (==) `on` (matchupGamesPlayed . unNumberOfMatchesMatchup)

instance Ord NumberOfMatches where
  compare = compare `on` (matchupGamesPlayed . unNumberOfMatchesMatchup)

instance Show NumberOfMatches where
  show = (show . matchupGamesPlayed . unNumberOfMatchesMatchup)
