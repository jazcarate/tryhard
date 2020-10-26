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
import           Data.List                      ( sort )
import           Tryhard.Types

run :: IO ()
run = do
  config                 <- defaultConfig "tryhard"
  appConfig :: AppConfig <- getFromRootConfig config

  heroes                 <- getHeroes appConfig
  let hero = heroes !! 4

  m                 <- newMatchupMatrix appConfig
  resp :: [Matchup] <- m `for` (heroID hero)

  let wp = take 4 $ reverse $ sort $ NumberOfMatches <$> resp

  -- _ <- start heroes
  putStrLn $ "Best matchups for " <> show hero
  putStrLn $ "By NumberOfMatches"
  putStrLn $ show $ (\x -> (bindHeros heroes $ getHero x, x)) <$> wp
