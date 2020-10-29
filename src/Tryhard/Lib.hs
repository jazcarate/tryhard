{-# LANGUAGE ScopedTypeVariables #-}

module Tryhard.Lib where

import           Conferer                       ( defaultConfig
                                                , getFromRootConfig
                                                )

import           Tryhard.Config                 ( AppConfig )
import           Tryhard.Stats
import           Tryhard.Stats.Mode
import           Tryhard.OpenDota
import           Tryhard.OpenDota.HeroDB
import           Tryhard.Types
import           Tryhard.Engine


run :: IO ()
run = do
  config                 <- defaultConfig "tryhard"
  appConfig :: AppConfig <- getFromRootConfig config

  heroes                 <- getHeroes appConfig
  hero1                  <-
    maybe (fail "The hero was not found") (pure . heroTC) $ heroes `byNameLike` "Treant"

  hero2 <-
    maybe (fail "The hero was not found") (pure .heroTC) $ heroes `byNameLike` "lion"

  let composition = with hero2 $ with hero1 $ comp
  m     <- newMatchupMatrix appConfig heroes
  resp1 <- m `for` (All composition)

  let wp = take 4 $ recomend $ getMax <$> (Max <$> numberOfMatches <$> resp1)
  -- _ <- start heroes
  putStrLn $ "Best matchups for " <> show composition
  putStrLn $ "By Max <$> WinPercentage"
  putStrLn $ show wp

for :: MatchupMatrix -> LookAt -> IO (StatsResult Matchup)
for matrix lookAt = do
  let heroes = toList lookAt
  results <- sequence $ forHero matrix <$> heroes
  pure $ foldl mappend mempty results