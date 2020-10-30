module Tryhard.Stats where

import qualified Data.HashMap.Strict           as HM
import           Data.Functor.Identity          ( Identity
                                                , runIdentity
                                                )
import           Data.List                      ( delete )
import           Data.Functor.Compose           ( getCompose
                                                , Compose(Compose)
                                                )
import           Control.Concurrent.STM         ( modifyTVar
                                                , atomically
                                                , readTVarIO
                                                , newTVarIO
                                                )
import           Data.Hashable                  ( Hashable )

import           Tryhard.Config
import           Tryhard.Types
import           Tryhard.OpenDota
import           Tryhard.OpenDota.HeroDB

import           Debug.Trace

type UnderlyingMatchupMatrix = HM.HashMap Hero (StatsResult Matchup)

-- TODO if StatsResult is traversable, m can be "inside" a and sequence after to not keep track of `m`
newtype Stats m a = Stats { runStats :: MatchComp -> m (StatsResult a) }

instance (Functor m) => Functor (Stats m) where
  fmap f (Stats r) = Stats $ \h -> getCompose $ f <$> Compose (r h)

cached :: (Hashable a, Eq a) => (a -> IO res) -> IO (a -> IO res)
cached f = do
  cacheT <- newTVarIO HM.empty
  pure $ \what -> do
    cache <- readTVarIO $ cacheT
    let val = HM.lookup what cache
    case val of
      Just x  -> trace "HIT" $ pure x
      Nothing -> trace "Miss :(" $ do
        response <- f what
        _        <- atomically $ modifyTVar cacheT (HM.insert what response)
        pure response

forHeroMatchup :: AppConfig -> HeroDB -> IO (Hero -> IO (StatsResult Matchup))
forHeroMatchup config heroDB = cached $ getHeroMatchup config heroDB

withMatchup
  :: (Semigroup a) => AppConfig -> HeroDB -> (Matchup -> a) -> Stats IO a
withMatchup config heroDB f = Stats $ \heroes -> do
  forOne <- forHeroMatchup config heroDB
  let statsL = forOne <$> toList heroes
  statsM <- sequence statsL
  let stats = getCompose $ f <$> Compose statsM
  pure $ mconcat stats

withCombo :: AppConfig -> HeroDB -> Stats IO Combo
withCombo config heroDB = Stats $ \matchComp -> do
  x <- cached $ getHeroCombo config heroDB
  x matchComp

withConst
  :: (Semigroup a) => HM.HashMap Hero (StatsResult a) -> Stats Identity a
withConst matrix = Stats $ \heroes -> pure $ mconcat $ forOne <$> toList heroes
  where forOne hero = maybe mempty id $ HM.lookup hero matrix

class (Monad m) => ToIO m where
  lift :: m a -> IO a

instance ToIO IO where
  lift = id

instance ToIO Identity where
  lift = pure . runIdentity

-- Maybe this shouldb e a lazy map?, as we are AxA each hero
constHeroDB :: HeroDB -> HM.HashMap Hero (StatsResult Hero)
constHeroDB db = HM.fromList $ (\h -> (h, forOne h)) <$> allHeros
 where
  allHeros = findAll db
  forOne :: Hero -> StatsResult Hero
  forOne hero = unLHS <$> (statsFromList $ dup <$> (delete hero $ findAll db))
  dup :: a -> (LHS a, a)
  dup a = (LHS a, a)
