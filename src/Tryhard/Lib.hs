module Tryhard.Lib where

import           Conferer                       ( defaultConfig
                                                , getFromRootConfig
                                                )

import qualified Data.Text                     as T
import           Data.List                      ( intercalate )
import qualified Data.HashMap.Strict           as HM
import           Data.Functor.Identity          ( Identity )
import           Control.Concurrent.STM         ( newTVarIO )

import           Tryhard.Stats
import           Tryhard.Stats.Mode
import           Tryhard.OpenDota
import           Tryhard.OpenDota.HeroDB
import           Tryhard.Types
import           Tryhard.Engine


readHero :: HeroDB -> IO [Hero]
readHero db = go []
 where
  go :: [Hero] -> IO [Hero]
  go acc = do
    putStrLn $ "Hero " <> (show $ length acc) <> ": "
    line' <- getLine
    let line = T.pack line'
    case line of
      ""  -> pure acc
      str -> do
        maybe (go acc) (\h -> go (h : acc)) $ db `byNameLike` str

-- TODO: this is tail end recursion, no?
choose :: String -> [(String, a)] -> IO a
choose what choices = do
  putStrLn $ what <> ": " <> intercalate "," (fst <$> choices)
  line <- getLine
  case lookup line choices of -- TODO more inteligente lookup
    Nothing -> do
      putStrLn $ "Coudn't find that " <> what <> ". Try again."
      choose what choices
    Just chosen -> pure chosen

data What = WhatWinPercengate (Stats IO WinPercentage)
  | WhatLegs (Stats Identity NumberOfLegs)
  | WhatMatches (Stats IO NumberOfMatches)

data How = HowSum | HowMax

-- TODO yuc!
recomendBy :: IO How -> IO What -> [Hero] -> IO [Result]
recomendBy how what heroes = do
  what' <- what
  case what' of
    WhatLegs    s -> foo $ s
    WhatMatches s -> do
      how' <- how
      case how' of
        HowSum -> foo $ Sum <$> s
        HowMax -> foo $ Max <$> s
    WhatWinPercengate s -> do
      how' <- how
      case how' of
        HowSum -> foo $ Sum <$> s
        HowMax -> foo $ Max <$> s
 where
  foo :: (Show a, Ord a, ToIO m) => Stats m (a) -> IO [Result]
  foo s = do
    x <- lift $ runStats s heroes
    pure $ recomend x

run :: IO ()
run = do
  config    <- defaultConfig "tryhard"
  appConfig <- getFromRootConfig config

  db        <- getHeroes appConfig

  heroes    <- readHero db
  let composition = foldl (flip with) comp $ heroTC <$> heroes
  putStrLn $ "Recomendarion for " <> show composition

  -- TODO extract init to a outside function?
  matchupCache <- newTVarIO HM.empty

  let matchup = withMatchup appConfig db matchupCache
  let heros   = withConst $ constHeroDB db

  let what = choose
        "what"
        [ ("matches", WhatMatches $ numberOfMatches <$> matchup)
        , ("wins"   , WhatWinPercengate $ WinPercentage <$> matchup)
        , ("legs"   , WhatLegs $ numberOfLegs <$> heros)
        ]

  let how = choose "how" [("sum", HowSum), ("max", HowMax)]

  resp <- recomendBy how what $ toList composition
  putStrLn $ show resp
