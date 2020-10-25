{-# LANGUAGE ScopedTypeVariables #-}

module Tryhard.Lib where

import           Conferer
import           Tryhard.Config                 ( AppConfig )
import           Tryhard.OpenDota               ( getHeroes )
import           Tryhard.TUI                    ( start )

main :: IO ()
main = do
  config                 <- defaultConfig "tryhard"
  appConfig :: AppConfig <- getFromRootConfig config

  heroes                 <- getHeroes appConfig
  _ <- start heroes
  putStrLn "Bye!"

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

bindHeros :: [HeroMatchupResponse] -> [HeroResponse] -> [Text]
bindHeros matchups heroes = withName <$> matchups
 where
  withName :: HeroMatchupResponse -> Text
  withName m =
    maybe "-" heroName $ find (\hs -> heroID hs == heroMatchupHeroID m) heroes

-}
