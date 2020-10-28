{-# LANGUAGE MultiParamTypeClasses #-}

module Tryhard.Stats where

import           Control.Concurrent.STM
import qualified Data.HashMap.Strict           as HM
import           Data.Functor.Identity          ( Identity )
import           Data.List                      ( delete )

import           Tryhard.Config
import           Tryhard.Types
import           Tryhard.OpenDota
import           Tryhard.OpenDota.HeroDB



type UnderlyingMatchupMatrix = HM.HashMap Hero [Matchup]
-- TODO: Betternames to the Stats Intances
data MatchupMatrix = MatchupMatrix {
  matchupMatrixHeroDB :: HeroDB,
  matchupMatrixContainer :: TVar UnderlyingMatchupMatrix,
  matchupMatrixAppConfig :: AppConfig
}

instance Stats MatchupMatrix IO Matchup where
  for matrix hero = do
    cache <- readTVarIO $ container
    let val = HM.lookup hero cache
    case val of
      Just x  -> pure $ statsResult hero x
      Nothing -> do
        response <- getHeroMatchup (matchupMatrixAppConfig matrix)
                                   (matchupMatrixHeroDB matrix)
                                   hero
        _ <- atomically $ modifyTVar container (HM.insert hero response)
        pure $ statsResult hero response
    where container = matchupMatrixContainer matrix

newMatchupMatrix :: AppConfig -> HeroDB -> IO MatchupMatrix
newMatchupMatrix config heroDB = do
  m <- newTVarIO HM.empty
  return MatchupMatrix { matchupMatrixContainer = m
                       , matchupMatrixAppConfig = config
                       , matchupMatrixHeroDB    = heroDB
                       }

-- For testing pourposes. Shou
newtype ConstMathcupMap = ConstMathcupMap UnderlyingMatchupMatrix

instance Stats ConstMathcupMap Identity Matchup where
  for (ConstMathcupMap map') hero = maybe mempty pure f
   where
    f :: Maybe (StatsResult Matchup)
    f = do
      matchups <- HM.lookup hero map'
      pure $ statsResult hero matchups

newConstMatchupMatrix :: UnderlyingMatchupMatrix -> ConstMathcupMap
newConstMatchupMatrix = ConstMathcupMap

newtype HeroStat = HeroStat HeroDB

instance Stats HeroStat Identity Hero where
  for (HeroStat db) hero = pure $ statsResult hero $ delete hero $ findAll db
