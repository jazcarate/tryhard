module Tryhard.Stats where

import qualified Data.HashMap.Strict           as HM
import           Data.Functor.Identity          ( Identity
                                                , runIdentity
                                                )
import           Data.Functor.Compose           ( getCompose
                                                , Compose(Compose)
                                                )
import           Control.Concurrent.STM         ( modifyTVar
                                                , atomically
                                                , readTVarIO
                                                , newTVarIO
                                                )
import           Data.Hashable                  ( Hashable )
import           Data.Maybe                     ( catMaybes )
import           Data.Algebra.Free              ( FreeSemiGroup(FreeSemiGroup) )

import           Tryhard.Config
import           Tryhard.Types
import           Tryhard.OpenDota
import           Tryhard.OpenDota.HeroDB
import           Tryhard.Stats.Mode

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
      Just x  -> pure x
      Nothing -> do
        response <- f what
        _        <- atomically $ modifyTVar cacheT (HM.insert what response)
        pure response

forHeroMatchup :: AppConfig -> HeroDB -> IO (Hero -> IO (StatsResult Matchup))
forHeroMatchup config heroDB = cached $ getHeroMatchup config heroDB

withMatchup
  :: AppConfig
  -> HeroDB
  -> IO (Stats IO (FreeSemiGroup (KeepHero (Foo Matchup))))
withMatchup config heroDB = do
  forOne <- forHeroMatchup config heroDB
  pure $ Stats $ \heroes -> do
    let keep f h = (\val -> KeepHero h <$> f <$> val) <$> forOne h
    let (myTeam, enemyTeam) = toTuple heroes
    let statsL = ((keep Id) <$> myTeam) <> ((keep Invert) <$> enemyTeam)
    statsM <- sequence statsL
    let stats = getCompose $ FreeSemiGroup <$> Compose statsM
    pure $ mconcat stats

withCombo :: AppConfig -> HeroDB -> IO (Stats IO Combo)
withCombo config heroDB = do
  forOne <- cached $ getHeroCombo config heroDB
  pure $ Stats forOne

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
constHeroDB
  :: HeroDB
  -> (Hero -> Hero -> Maybe a)
  -> HM.HashMap Hero (StatsResult (FreeSemiGroup a))
constHeroDB db f = HM.fromList $ (\h -> (h, forOne h)) <$> allHeros
 where
  allHeros = findAll db
  forOne hero = (statsFromList $ catMaybes $ dup hero <$> findAll db)
  dup a b = (\c -> (FreeSemiGroup c, b)) <$> (f b a)

-- TODO really need to clean up packages
data DataSources = DataSources
  { dataSourceHeroDB :: HeroDB
  , dataSourceMatchup :: Stats IO (FreeSemiGroup (KeepHero (Foo Matchup)))
  , dataSourceNumberOfLegs :: Stats Identity (FreeSemiGroup NumberOfLegs)
  , dataSourceCombo :: Stats IO Combo
  }
