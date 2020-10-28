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
    maybe (fail "The hero was not found") pure $ heroes `byNameLike` "Treant"

  hero2 <-
    maybe (fail "The hero was not found") pure $ heroes `byNameLike` "lion"

  m     <- newMatchupMatrix appConfig heroes
  resp1 <- m `for` hero1
  resp2 <- m `for` hero2

  let wp =
        take 4
          $   recomend
          $   unMax
          <$> (Max <$> WinPercentage <$> resp1)
          <>  (Max <$> WinPercentage <$> resp2)
  -- _ <- start heroes
  putStrLn $ "Best matchups for " <> show hero1 <> " and " <> show hero2
  putStrLn $ "By Max <$> WinPercentage"
  putStrLn $ show wp
