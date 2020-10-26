{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}

module Tryhard.OpenDota where


import           Tryhard.Config
import           System.FilePath
import           Tryhard.OpenDota.Internal
import           Data.Text                      ( Text )
import           Network.HTTP.Req
import           Control.Monad.Catch
import           Text.URI                       ( mkURI )
import           Tryhard.Types
import           Control.Concurrent.STM
import qualified Data.HashMap.Strict           as HM
import           Data.Functor.Identity          ( Identity )


heroesETagFile :: FilePath -> FilePath
heroesETagFile home = home </> "heroes.etag"

heroesJSONFile :: FilePath -> FilePath
heroesJSONFile home = home </> "heroes.json"


getHeroes :: AppConfig -> IO ([Hero])
getHeroes appConfig = do
  url  <- prepareUrl (appConfigHeroJsonURL appConfig)
  home <- toPath $ appConfigHome appConfig
  let etagFilePath   = heroesETagFile home
  let heroesFilePath = heroesJSONFile home
  response <- getHerosWithCache etagFilePath heroesFilePath url
  pure $ toHero <$> response

type URL = (Url 'Https, Option 'Https)

data MatchupMatrix = MatchupMatrix {
  matchupMatrixContainer :: TVar UnderlyingMatchupMatrix,
  matchupMatrixUrl :: URL
}

class (Monad m) => MatchupMap a m where
  for :: a -> HeroID -> m [Matchup]

instance MatchupMap MatchupMatrix IO where
  for matrix heroId = do
    cache <- readTVarIO $ container
    let val = HM.lookup heroId cache
    case val of
      Just x  -> pure x
      Nothing -> do
        resp' <- getHeroMatchup (unHero heroId) (matchupMatrixUrl matrix)
        let resp = toMatchup <$> resp'
        _ <- atomically $ modifyTVar container (HM.insert heroId resp)
        pure resp
    where container = matchupMatrixContainer matrix

type UnderlyingMatchupMatrix = HM.HashMap HeroID [Matchup]
newtype ConstMathcupMap = ConstMathcupMap UnderlyingMatchupMatrix

instance MatchupMap ConstMathcupMap Identity where
  for (ConstMathcupMap map') heroId = maybe mempty pure $ HM.lookup heroId map'

newConstMatchupMatrix :: UnderlyingMatchupMatrix -> ConstMathcupMap
newConstMatchupMatrix = ConstMathcupMap

newMatchupMatrix :: AppConfig -> IO MatchupMatrix
newMatchupMatrix config = do
  url <- prepareUrl (appConfigOpenDotaApi config)
  m   <- newTVarIO HM.empty
  return MatchupMatrix { matchupMatrixContainer = m, matchupMatrixUrl = url }

prepareUrl :: (MonadFail m, MonadThrow m) => Text -> m URL
prepareUrl url = do
  uri <- mkURI $ url
  maybe (fail "could not parse the URL") (pure) $ useHttpsURI uri

toHero :: HeroResponse -> Hero
toHero response = Hero { heroName = heroResponseName response
                       , heroID   = toHeroId $ heroResponseID response
                       }

toHeroId :: HeroIDResponse -> HeroID
toHeroId = HeroID . unHeroID

toMatchup :: HeroMatchupResponse -> Matchup
toMatchup response = Matchup
  { matchupHeroId      = toHeroId $ heroMatchupResponseHeroID response
  , matchupGamesPlayed = heroMatchupGamesResponsePlayed response
  , matchupWins        = heroMatchupResponseWins response
  }
