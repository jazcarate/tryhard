{-# LANGUAGE ScopedTypeVariables #-}

module Tryhard.Lib where

import           Conferer                       ( defaultConfig
                                                , getFromRootConfig
                                                )
import           Tryhard.Config                 ( AppConfig )
import           Data.List                      ( sort )
-- import           Data.Functor.Identity          ( Identity(runIdentity) )

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
  hero1                  <-
    maybe (fail "The hero was not found") pure $ heroes `byNameLike` "Treant"

  hero2 <-
    maybe (fail "The hero was not found") pure $ heroes `byNameLike` "lion"

  m     <- newMatchupMatrix appConfig heroes
  resp1 <- m `for` hero1
  resp2 <- m `for` hero2

  let wp =
        take 4
          $ reverse
          $ sort
          $ toList
          $ ((WinPercentage <$> resp1) <> (WinPercentage <$> resp2))
  -- _ <- start heroes
  putStrLn $ "Best matchups for " <> show hero1 <> " and " <> show hero2
  putStrLn $ "By NumberOfLegs"
  putStrLn
    $   show
    $   (\x -> (unInnerResultHero x, unInnerResult x, getHero x))
    <$> wp
