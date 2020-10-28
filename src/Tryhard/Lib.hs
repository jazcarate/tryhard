{-# LANGUAGE ScopedTypeVariables #-}

module Tryhard.Lib where

import           Conferer                       ( defaultConfig
                                                , getFromRootConfig
                                                )
import           Tryhard.Config                 ( AppConfig )
import           Data.List                      ( sort )
import           Data.Functor.Identity          ( Identity(runIdentity) )

import           Tryhard.Stats
import           Tryhard.Stats.Mode
import           Tryhard.OpenDota
import           Tryhard.OpenDota.HeroDB
import           Tryhard.Types

run :: IO ()
run = do
  config                 <- defaultConfig "tryhard"
  appConfig :: AppConfig <- getFromRootConfig config

  heroes                 <- getHeroes appConfig
  hero                   <-
    maybe (fail "The hero was not found") pure $ heroes `byNameLike` "Broodmother"

  let m    = HeroStat heroes
  let resp = runIdentity $ m `for` hero

  let wp = take 4 $ reverse $ sort $ NumberOfLegs <$> resp

  -- _ <- start heroes
  putStrLn $ "Best matchups for " <> show hero
  putStrLn $ "By NumberOfLegs"
  putStrLn $ show $ (\x -> (getHero x, x)) <$> wp
