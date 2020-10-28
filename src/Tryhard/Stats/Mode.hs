module Tryhard.Stats.Mode
  ( module Tryhard.Stats.Mode
  , module Data.Semigroup
  )
where

import           Data.Ratio                     ( Ratio
                                                , denominator
                                                , numerator
                                                , (%)
                                                )
import           Numeric                        ( showGFloat )
import           Data.Function                  ( on )
import           Data.Semigroup                 ( Max(Max, getMax) )

import           Tryhard.Types

newtype WinPercentage = WinPercentage { unWinPercentageMatchup :: Matchup }

instance Eq WinPercentage where
  (==) = (==) `on` (winRate . unWinPercentageMatchup)

instance Ord WinPercentage where
  compare = compare `on` (winRate . unWinPercentageMatchup)

newtype SumWinPercentage = SumWinPercentage { unSumWinPercentage :: WinPercentage }

instance Semigroup (SumWinPercentage) where
  (SumWinPercentage (WinPercentage m1)) <> (SumWinPercentage (WinPercentage m2))
    = SumWinPercentage $ WinPercentage $ sumMatchups m1 m2

newtype KeepHero a = KeepHero { unKeepHero :: a }

instance Ord WinRate where
  compare NoMatches       NoMatches       = EQ
  compare NoMatches       (SomeMatches _) = LT
  compare (SomeMatches _) NoMatches       = GT
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
newtype NumberOfMatches = NumberOfMatches { unNumberOfMatches :: Int } deriving (Eq, Ord)

numberOfMatches :: Matchup -> NumberOfMatches
numberOfMatches = NumberOfMatches . matchupGamesPlayed

instance Show NumberOfMatches where
  show = show . unNumberOfMatches

newtype SumNumberOfMatches = SumNumberOfMatches { unSumNumberOfMatches :: NumberOfMatches } deriving (Eq, Show)

newtype Sum a = Sum { getSum :: a } deriving (Eq, Show)

class Summable a where
  (<+>) :: a -> a -> a

instance Summable NumberOfMatches where
  (NumberOfMatches a) <+> (NumberOfMatches b) = NumberOfMatches $ a <+> b

instance Summable Int where
  (<+>) = (+)

instance Summable a => Semigroup (Sum a) where
  (Sum a) <> (Sum b) = Sum $ a <+> b

-- This might be the <> for Matchup... but something doesen't sit right
sumMatchups :: Matchup -> Matchup -> Matchup
sumMatchups (Matchup { matchupGamesPlayed = played1, matchupWins = wins1 }) (Matchup { matchupGamesPlayed = played2, matchupWins = wins2 })
  = Matchup { matchupGamesPlayed = played1 + played2
            , matchupWins        = wins1 + wins2
            }

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
