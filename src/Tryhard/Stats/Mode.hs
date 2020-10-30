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
  show = (\s -> s ++ " matches") . (show . unNumberOfMatches)

newtype Sum a = Sum { getSum :: a } deriving (Eq, Ord)

instance Show a => Show (Sum a) where
  show (Sum a) = show a

newtype Max a = Max { getMax :: a } deriving (Eq, Ord)

instance Ord a => Semigroup (Max a) where
  a <> b = case (compare `on` getMax) a b of
    EQ -> a
    GT -> a
    LT -> b

instance Show a => Show (Max a) where
  show (Max a) = show a

class Summable a where
  (<+>) :: a -> a -> a

instance Summable NumberOfMatches where
  (NumberOfMatches a) <+> (NumberOfMatches b) = NumberOfMatches $ a <+> b

instance Summable Int where
  (<+>) = (+)

instance Summable a => Semigroup (Sum a) where
  (Sum a) <> (Sum b) = Sum $ a <+> b

instance Summable Matchup where
  (Matchup { matchupGamesPlayed = p1, matchupWins = w1 }) <+> (Matchup { matchupGamesPlayed = p2, matchupWins = w2 })
    = Matchup (p1 + p2) (w1 + w2)

instance Summable WinPercentage where
  (WinPercentage a) <+> (WinPercentage b) = WinPercentage $ a <+> b

---------------------------------
newtype NumberOfLegs = NumberOfLegs { unNumberOfLegs :: Int } deriving (Eq, Ord, Bounded)

numberOfLegs :: Hero -> NumberOfLegs
numberOfLegs = NumberOfLegs . heroLegs

instance Show NumberOfLegs where
  show = (\s -> s ++ " legs") . (show . unNumberOfLegs)

instance Semigroup NumberOfLegs where
  (NumberOfLegs a) <> (NumberOfLegs b) = NumberOfLegs (a + b)
---------------------------------
---------------------------------
