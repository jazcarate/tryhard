{-# LANGUAGE ScopedTypeVariables #-}

module Tryhard.Lib where

import           Conferer
import           Tryhard.Config                 ( AppConfig )
import           Tryhard.OpenDota
import           Data.List                      ( sort )
import           Tryhard.Types
import           Tryhard.OpenDota.Internal      ( byNameLike )

run :: IO ()
run = do
  config                 <- defaultConfig "tryhard"
  appConfig :: AppConfig <- getFromRootConfig config

  heroes                 <- getHeroes appConfig
  hero                   <-
    maybe (fail "The hero was not found") pure $ heroes `byNameLike` "Razor"

  m    <- newMatchupMatrix appConfig heroes
  resp <- m `for` hero

  let wp = take 4 $ reverse $ sort $ WinPercentage <$> resp

  -- _ <- start heroes
  putStrLn $ "Best matchups for " <> show hero
  putStrLn $ "By WinPercentage"
  putStrLn $ show $ (\x -> (getHero x, x)) <$> wp

recomend
  :: (Stats container m result)
  => container
  -> [Hero]
  -> [Hero]
  -> m [(Hero, result)]
recomend = undefined
