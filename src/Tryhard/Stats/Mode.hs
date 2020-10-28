module Tryhard.Stats.Mode where

import           Data.Ratio                     ( Ratio
                                                , denominator
                                                , numerator
                                                , (%)
                                                )
import           Numeric                        ( showGFloat )
import           Data.Function                  ( on )

import           Tryhard.Types

newtype WinPercentage = WinPercentage { unWinPercentageMatchup :: Matchup }

instance WithHero WinPercentage where
  getHero = matchupHero . unWinPercentageMatchup

instance Eq WinPercentage where
  (==) = (==) `on` unWinPercentageMatchup

instance Ord WinPercentage where
  compare = compare `on` (winRate . unWinPercentageMatchup)

instance Ord WinRate where
  compare NoMatches       NoMatches       = EQ
  compare NoMatches       (SomeMatches _) = GT
  compare (SomeMatches _) NoMatches       = LT
  compare (SomeMatches a) (SomeMatches b) = compare a b

data WinRate = NoMatches | SomeMatches (Ratio Int) deriving (Eq)

winRate :: Matchup -> WinRate
winRate m = case matchupGamesPlayed m of
  0      -> NoMatches
  played -> SomeMatches $ (matchupWins m) % played

instance Show WinPercentage where
  show wp = case winRate $ unWinPercentageMatchup wp of
    NoMatches         -> "-%"
    SomeMatches ratio -> showRatio "%" $ ratio * 100

showRatio :: String -> Ratio Int -> String
showRatio pos r = showGFloat (Just 2) val pos
 where
  val :: Float
  val = (realToFrac $ numerator r) / (realToFrac $ denominator r)


---------------------------------
newtype NumberOfMatches = NumberOfMatches { unNumberOfMatchesMatchup :: Matchup }

instance Eq NumberOfMatches where
  (==) = (==) `on` (matchupGamesPlayed . unNumberOfMatchesMatchup)

instance Ord NumberOfMatches where
  compare = compare `on` (matchupGamesPlayed . unNumberOfMatchesMatchup)

instance Show NumberOfMatches where
  show = (show . matchupGamesPlayed . unNumberOfMatchesMatchup)

instance WithHero NumberOfMatches where
  getHero = matchupHero . unNumberOfMatchesMatchup
