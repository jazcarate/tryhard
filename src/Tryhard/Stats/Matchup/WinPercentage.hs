module Tryhard.Stats.Matchup.WinPercentage where

import           Data.Ratio                     ( Ratio
                                                , denominator
                                                , numerator
                                                , (%)
                                                )
import           Numeric                        ( showGFloat )
import           Data.Function                  ( on )

import           Tryhard.Stats.Types
import           Tryhard.Stats.Matchup

newtype WinPercentage = WinPercentage { unWinPercentageMatchup :: Matchup }

instance Eq WinPercentage where
  (==) = (==) `on` (winRate . unWinPercentageMatchup)

instance Ord WinPercentage where
  compare = compare `on` (winRate . unWinPercentageMatchup)

instance Ord WinRate where
  compare NoMatches       NoMatches       = EQ
  compare NoMatches       (SomeMatches _) = LT
  compare (SomeMatches _) NoMatches       = GT
  compare (SomeMatches a) (SomeMatches b) = compare a b


instance Show WinPercentage where
  show wp = case winRate $ unWinPercentageMatchup wp of
    NoMatches         -> "-%"
    SomeMatches ratio -> showRatio "%" $ ratio * 100

instance Invertable WinPercentage where
  invert (WinPercentage Matchup { matchupGamesPlayed = played, matchupWins = wins })
    = WinPercentage
      $ Matchup { matchupGamesPlayed = played, matchupWins = (played - wins) }

data WinRate = NoMatches | SomeMatches (Ratio Int) deriving (Eq)

winRate :: Matchup -> WinRate
winRate m = case matchupGamesPlayed m of
  0      -> NoMatches
  played -> SomeMatches $ (matchupWins m) % played


showRatio :: String -> Ratio Int -> String
showRatio pos r = showGFloat (Just 2) val pos
 where
  val :: Float
  val = (realToFrac $ numerator r) / (realToFrac $ denominator r)

instance Summable WinPercentage where
  (WinPercentage a) <+> (WinPercentage b) = WinPercentage $ a <++> b
   where
    (Matchup { matchupGamesPlayed = p1, matchupWins = w1 }) <++> (Matchup { matchupGamesPlayed = p2, matchupWins = w2 })
      = Matchup (p1 + p2) (w1 + w2)
