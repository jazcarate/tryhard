module Tryhard.Lib where

import           Conferer                       ( defaultConfig
                                                , getFromRootConfig
                                                )

import qualified Data.Text                     as T
import           Data.Functor.Identity          ( Identity )
import           Data.Algebra.Free              ( FreeSemiGroup )
import           Control.Concurrent.STM         ( modifyTVar
                                                , atomically
                                                , readTVarIO
                                                , newTVarIO
                                                )
import           Data.Hashable                  ( Hashable )
import           Data.Maybe                     ( catMaybes )
import           Data.Algebra.Free              ( FreeSemiGroup(FreeSemiGroup) )
import qualified Data.HashMap.Strict           as HM

import           Tryhard.Config
import           Tryhard.Types
import           Tryhard.OpenDota
import qualified Tryhard.OpenDota.HeroDB       as DB

import           Tryhard.Hero
import qualified Tryhard.Picks                 as P
import           Tryhard.Stats
import           Tryhard.Stats.Result
import           Tryhard.Stats.Matchup
import           Tryhard.Stats.NumberOfLegs
import           Tryhard.Stats.Combo
import           Tryhard.Stats.Types
import           Tryhard.TUI
import           Data.Functor.Compose           ( getCompose
                                                , Compose(Compose)
                                                )

type UnderlyingMatchupMatrix = HM.HashMap Hero (Result Matchup)

cached :: (Hashable a, Eq a) => (a -> IO res) -> IO (a -> IO res)
cached f = do
  cacheT <- newTVarIO HM.empty
  pure $ \what -> do
    cache <- readTVarIO $ cacheT
    let val = HM.lookup what cache
    case val of
      Just x  -> pure x
      Nothing -> do
        response <- f what
        _        <- atomically $ modifyTVar cacheT (HM.insert what response)
        pure response

forHeroMatchup :: AppConfig -> DB.HeroDB -> IO (Hero -> IO (Result Matchup))
forHeroMatchup config heroDB = cached $ getHeroMatchup config heroDB

withMatchup
  :: AppConfig
  -> DB.HeroDB
  -> IO (Stats IO (FreeSemiGroup (ShouldInvert Matchup)))
withMatchup config heroDB = do
  forOne <- forHeroMatchup config heroDB
  pure $ Stats $ \heroes -> do
    let keep f h = (f <$>) <$> forOne h
    let (allies, enemies) = P.teams heroes
    let statsL = ((keep DontInvert) <$> allies) <> ((keep Invert) <$> enemies)
    statsM <- sequence statsL
    let stats' = getCompose $ FreeSemiGroup <$> Compose statsM
    pure $ mconcat stats'

withCombo :: AppConfig -> DB.HeroDB -> IO (Stats IO Combo)
withCombo config heroDB = do
  forOne <- cached $ getHeroCombo config heroDB
  pure $ Stats forOne

withConst :: (Semigroup a) => HM.HashMap Hero (Result a) -> Stats Identity a
withConst matrix = Stats
  $ \heroes -> pure $ mconcat $ forOne <$> P.teamsList heroes
  where forOne hero = maybe mempty id $ HM.lookup hero matrix

-- Maybe this shouldb e a lazy map?, as we are AxA each hero
constHeroDB
  :: DB.HeroDB
  -> (Hero -> Hero -> Maybe a)
  -> HM.HashMap Hero (Result (FreeSemiGroup a))
constHeroDB db f = HM.fromList $ (\h -> (h, forOne h)) <$> allHeros
 where
  allHeros = DB.findAll db
  forOne hero = (fromList $ catMaybes $ dup hero <$> DB.findAll db)
  dup a b = (\c -> (FreeSemiGroup c, b)) <$> (f b a)

readHero :: DB.HeroDB -> IO [Hero]
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
        maybe (go acc) (\h -> go (h : acc)) $ db `DB.byNameLike` str

skipSelf :: (Hero -> Hero -> a) -> Hero -> Hero -> Maybe a
skipSelf f a b = if a == b then Nothing else Just $ f a b

run :: IO ()
run = do

  config    <- defaultConfig "tryhard"
  appConfig <- getFromRootConfig config

  db        <- getHeroes appConfig

  matchup   <- withMatchup appConfig db
  let legged = withConst $ constHeroDB db (skipSelf numberOfLegs)
  combos <- withCombo appConfig db

  _      <- start (DataSources db matchup legged combos)
  putStrLn "Bye"
