module Tryhard.Lib where

import           Conferer                       ( defaultConfig
                                                , getFromRootConfig
                                                )

import           Tryhard.Stats
import           Tryhard.Stats.Mode
import           Tryhard.OpenDota
import           Tryhard.OpenDota.HeroDB
import           Tryhard.Types
import           Tryhard.Engine


run :: IO ()
run = do
  config    <- defaultConfig "tryhard"
  appConfig <- getFromRootConfig config

  heroes    <- getHeroes appConfig
  hero1     <-
    maybe (fail "The hero was not found") (pure . heroTC)
    $            heroes
    `byNameLike` "Treant"

  hero2 <-
    maybe (fail "The hero was not found") (pure . heroTC)
    $            heroes
    `byNameLike` "lion"

  let composition = with hero2 $ against hero1 $ comp
  m     <- withMatchup appConfig heroes (Sum . numberOfMatches)
  resp1 <- m (toList $ composition)

  let wp = take 4 $ recomend resp1
  -- _ <- start heroes
  putStrLn $ "Best matchups for " <> show composition
  putStrLn $ "By Max <$> WinPercentage"
  putStrLn $ show wp
