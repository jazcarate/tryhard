module Tryhard.Stats.Matchup.NumberOfMatches where

import           Tryhard.Stats.Types
import           Tryhard.Stats.Matchup

newtype NumberOfMatches = NumberOfMatches { unNumberOfMatches :: Int } deriving (Eq, Ord)

numberOfMatches :: Matchup -> NumberOfMatches
numberOfMatches = NumberOfMatches . matchupGamesPlayed

instance Show NumberOfMatches where
  show = (\s -> s ++ " matches") . (show . unNumberOfMatches)

instance Invertable NumberOfMatches where
  invert = id

instance Summable NumberOfMatches where
  NumberOfMatches { unNumberOfMatches = a } <+> NumberOfMatches { unNumberOfMatches = b }
    = NumberOfMatches { unNumberOfMatches = a + b }
