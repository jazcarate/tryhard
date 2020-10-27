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
import           Data.Maybe                     ( catMaybes )


heroesETagFile :: FilePath -> FilePath
heroesETagFile home = home </> "heroes.etag"

heroesJSONFile :: FilePath -> FilePath
heroesJSONFile home = home </> "heroes.json"


getHeroes :: AppConfig -> IO HeroDB
getHeroes appConfig = do
  url  <- prepareUrl (appConfigHeroJsonURL appConfig)
  home <- toPath $ appConfigHome appConfig
  let etagFilePath   = heroesETagFile home
  let heroesFilePath = heroesJSONFile home
  getHerosWithCache etagFilePath heroesFilePath url

type URL = (Url 'Https, Option 'Https)

data MatchupMatrix = MatchupMatrix {
  matchupMatrixHeroDB :: HeroDB,
  matchupMatrixContainer :: TVar UnderlyingMatchupMatrix,
  matchupMatrixUrl :: URL
}

type Result a = (HeroID, a)


instance Stats MatchupMatrix IO Matchup where
  for matrix hero' = do
    cache <- readTVarIO $ container
    let val = HM.lookup hero' cache
    case val of
      Just x  -> pure x
      Nothing -> do
        rawResponse <- getHeroMatchup (unHero heroId) (matchupMatrixUrl matrix)
        let response = catMaybes $ toMatchup <$> rawResponse -- Ignore matchups that we can't find hero for
        _ <- atomically $ modifyTVar container (HM.insert hero' response)
        pure response
   where
    container = matchupMatrixContainer matrix
    heroId    = heroID hero'
    toMatchup :: HeroMatchupResponse -> Maybe Matchup
    toMatchup response = do
      hero'' <-
        (matchupMatrixHeroDB matrix)
          `byHeroId` (toheroID $ heroMatchupResponseHeroID response)
      pure $ Matchup
        { matchupHero        = hero''
        , matchupGamesPlayed = heroMatchupGamesResponsePlayed response
        , matchupWins        = heroMatchupResponseWins response
        }


type UnderlyingMatchupMatrix = HM.HashMap Hero [Matchup]
newtype ConstMathcupMap = ConstMathcupMap UnderlyingMatchupMatrix

instance Stats ConstMathcupMap Identity Matchup where
  for (ConstMathcupMap map') hero' = maybe mempty pure $ HM.lookup hero' map'

newConstMatchupMatrix :: UnderlyingMatchupMatrix -> ConstMathcupMap
newConstMatchupMatrix = ConstMathcupMap

newMatchupMatrix :: AppConfig -> HeroDB -> IO MatchupMatrix
newMatchupMatrix config heroDB = do
  url <- prepareUrl (appConfigOpenDotaApi config)
  m   <- newTVarIO HM.empty
  return MatchupMatrix { matchupMatrixContainer = m
                       , matchupMatrixUrl       = url
                       , matchupMatrixHeroDB    = heroDB
                       }

prepareUrl :: (MonadFail m, MonadThrow m) => Text -> m URL
prepareUrl url = do
  uri <- mkURI $ url
  maybe (fail "could not parse the URL") (pure) $ useHttpsURI uri
