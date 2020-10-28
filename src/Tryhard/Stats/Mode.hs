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

instance Eq WinPercentage where
  (==) = (==) `on` (winRate . unWinPercentageMatchup)

instance Ord WinPercentage where
  compare = compare `on` (winRate . unWinPercentageMatchup)

newtype SumWinPercentage = SumWinPercentage { unSumWinPercentage :: WinPercentage }

instance Semigroup (SumWinPercentage) where
  (SumWinPercentage (WinPercentage Matchup { matchupGamesPlayed = played1, matchupWins = wins1 })) <> (SumWinPercentage (WinPercentage Matchup { matchupGamesPlayed = played2, matchupWins = wins2 }))
    = SumWinPercentage $ WinPercentage $ Matchup
      { matchupGamesPlayed = played1 + played2
      , matchupWins        = wins1 + wins2
      }

newtype Max a = Max { unMax :: a }

instance Eq a => Eq (Max a) where
  (==) = (==) `on` unMax

instance Ord a => Ord (Max a) where
  compare = compare `on` unMax

instance Ord a => Semigroup (Max a) where
  m1 <> m2 = case compare m1 m2 of
    EQ -> m1
    GT -> m1
    LT -> m2

newtype KeepHero a = KeepHero { unKeepHero :: a }

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

---------------------------------
newtype NumberOfLegs = NumberOfLegs { unNumberOfLegsHero :: Hero }

instance Eq NumberOfLegs where
  (==) = (==) `on` (heroLegs . unNumberOfLegsHero)

instance Ord NumberOfLegs where
  compare = compare `on` (heroLegs . unNumberOfLegsHero)

instance Show NumberOfLegs where
  show = (\s -> s ++ " legs") . (show . heroLegs . unNumberOfLegsHero)

---------------------------------
---------------------------------
