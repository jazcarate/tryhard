module Tryhard.Lib where

import           Conferer                       ( defaultConfig
                                                , getFromRootConfig
                                                )

import qualified Data.Text                     as T
import           Data.List                      ( intercalate )
import           Data.Functor.Identity          ( Identity )
import           Data.Functor.Compose           ( Compose(Compose)
                                                , getCompose
                                                )

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

choose :: String -> [(String, a)] -> IO a
choose what choices = go
 where
  go = do
    putStrLn $ what <> ": " <> intercalate "," (fst <$> choices)
    line <- getLine
    case lookup line choices of -- TODO more inteligente lookup. Numbered? Default first?
      Nothing -> do
        putStrLn $ "Coudn't find that " <> what <> ". Try again."
        go
      Just chosen -> pure chosen

data What = WhatWinPercengate (Stats IO (Sum WinPercentage))
  | WhatLegs (Stats Identity NumberOfLegs)
  | WhatMatches (Stats IO (Sum NumberOfMatches))
  | WhatCombo (Stats IO Combo)

data How = HowSum | HowMax

data LookAt = LookAtAll | LookAtMyTeam | LookAtEnemyTeam

-- TODO yuc!
-- TODO LookAt!
recomendBy :: IO How -> IO What -> IO LookAt -> MatchComp -> IO [Result]
recomendBy how what _ matchcomp = do
  what' <- what
  case what' of
    WhatCombo   s -> foo matchcomp $ ByWith <$> s
    WhatLegs    s -> foo matchcomp $ s
    WhatMatches s -> do
      how' <- how
      case how' of
        HowSum -> foo matchcomp $ Sum <$> s
        HowMax -> foo matchcomp $ Max <$> s
    WhatWinPercengate s -> do
      how' <- how
      case how' of
        HowSum -> foo matchcomp $ Sum <$> s
        HowMax -> foo matchcomp $ Max <$> s
 where
  foo :: (Show a, Ord a, ToIO m) => MatchComp -> Stats m (a) -> IO [Result]
  foo c s = do
    x <- lift $ runStats s c
    pure $ recomend x

run :: IO ()
run = do
  config    <- defaultConfig "tryhard"
  appConfig <- getFromRootConfig config

  db        <- getHeroes appConfig

  heroes    <- readHero db
  let composition = foldl (flip with) comp $ heroTC <$> heroes
  putStrLn $ "Recomendarion for " <> show composition

  -- TODO Having the sum here is soooo wrong
  let matches = withMatchup appConfig db (Sum . numberOfMatches)
  let wins    = withMatchup appConfig db (Sum . WinPercentage)
  let legged =
        withConst $ getCompose $ numberOfLegs <$> Compose (constHeroDB db)
  let combos = withCombo appConfig db

  let what = choose
        "what"
        [ ("matches", WhatMatches $ matches)
        , ("wins"   , WhatWinPercengate $ wins)
        , ("legs"   , WhatLegs $ legged)
        , ("combos" , WhatCombo $ combos)
        ]

  let how = choose "how" [("sum", HowSum), ("max", HowMax)]

  let lookAt = choose
        "looking at"
        [ ("everyone"  , LookAtAll)
        , ("my team"   , LookAtMyTeam)
        , ("enemy team", LookAtEnemyTeam)
        ]

  go how what lookAt composition
 where
  go how what lookAt composition = do
    resp <- recomendBy how what lookAt composition
    putStrLn $ show resp
    go how what lookAt composition
