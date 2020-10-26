{-# LANGUAGE ScopedTypeVariables #-}

module Tryhard.Lib where

import           Conferer
import           Tryhard.Config                 ( AppConfig )
import           Tryhard.OpenDota               ( getHeroes
                                                , newMatchupMatrix
                                                , for
                                                )
import           Tryhard.TUI                    ( start )
import           Tryhard.Engine

import           Tryhard.Types

main :: IO ()
main = do
  config                 <- defaultConfig "tryhard"
  appConfig :: AppConfig <- getFromRootConfig config

  heroes                 <- getHeroes appConfig
  let hero = heroes !! 4

  m    <- newMatchupMatrix appConfig
  resp <- m `for` (heroID hero)

  -- _ <- start heroes
  putStrLn $ show $ length resp

{-
someFunc ::  Text -> IO ()
someFunc q = do
  config    <- defaultConfig "tryhard"
  appConfig <- getFromRootConfig config
  heroesUrl <- prepareUrl $ unHeroJsonUrl $ appConfigHeroJsonURL $ appConfig
  home      <- transformPath $ appConfigHome appConfig
  heros     <- getHerosWithCache home heroesUrl
  let f = Fuzzy.filter q (unList heros) "<" ">" heroName False
  let x = heroID $ Fuzzy.original $ head f
  openApiUrl <- prepareUrl $ unOpenDotaApiUrl $ appConfigOpenDotaApi $ appConfig
  matchupsRaw <- getHeroMatchup x openApiUrl
  let matchups = bindHeros matchupsRaw (unList heros)
  putStrLn $ show matchups
-}
