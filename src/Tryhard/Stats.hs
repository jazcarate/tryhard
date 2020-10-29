module Tryhard.Stats where

import           Control.Concurrent.STM
import qualified Data.HashMap.Strict           as HM
import           Data.Functor.Identity          ( Identity
                                                , runIdentity
                                                )
import           Data.List                      ( delete )

import           Tryhard.Config
import           Tryhard.Types
import           Tryhard.OpenDota
import           Tryhard.OpenDota.HeroDB
import           Data.Functor.Compose           ( getCompose
                                                , Compose(Compose)
                                                )



type UnderlyingMatchupMatrix = HM.HashMap Hero (StatsResult Matchup)

-- TODO if StatsResult is traversable, m can be "inside" a and sequence after to not keep track of `m`
newtype Stats m a = Stats { runStats :: [Hero] -> m (StatsResult a) }

instance (Functor m) => Functor (Stats m) where
  fmap f (Stats r) = Stats $ \h -> getCompose $ f <$> Compose (r h)

forHero
  :: AppConfig
  -> HeroDB
  -> TVar UnderlyingMatchupMatrix
  -> Hero
  -> IO (StatsResult Matchup)
forHero config heroDB container hero = do
  cache <- readTVarIO $ container
  let val = HM.lookup hero cache
  case val of
    Just x  -> pure x
    Nothing -> do
      response <- getHeroMatchup config heroDB hero
      _        <- atomically $ modifyTVar container (HM.insert hero response)
      pure response

withMatchup
  :: AppConfig -> HeroDB -> TVar UnderlyingMatchupMatrix -> Stats IO Matchup
withMatchup config heroDB container = Stats $ \heroes -> do
  let forOne = forHero config heroDB container
  let statsL = forOne <$> heroes
  stats <- sequence statsL
  pure $ mconcat stats

withConst :: Semigroup a => HM.HashMap Hero (StatsResult a) -> Stats Identity a
withConst matrix = Stats $ \heroes -> pure $ mconcat $ forOne <$> heroes
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
  forOne hero = statsFromList $ dup <$> (delete hero $ findAll db)
  dup :: a -> (a, a)
  dup a = (a, a)
