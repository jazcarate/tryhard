{-# LANGUAGE DataKinds #-}

module Tryhard.OpenDota
  ( getHeroes
  , getHeroMatchup
  , HeroResponse
  , HeroMatchupResponse
  )
where


import           Tryhard.Config
import           System.FilePath
import           Tryhard.OpenDota.Internal
import           Data.Text                      ( Text )
import           Network.HTTP.Req
import           Control.Monad.Catch
import           Text.URI                       ( mkURI )
import           Tryhard.Types


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

getHeroMatchup :: AppConfig -> HeroID -> IO ([HeroMatchupResponse])
getHeroMatchup appConfig heroId = do
  url <- prepareUrl (appConfigOpenDotaApi appConfig)
  getHeroMatchupInt id' url
  where id' = unHero heroId

prepareUrl
  :: (MonadFail m, MonadThrow m) => Text -> m (Url 'Https, Option scheme)
prepareUrl url = do
  uri <- mkURI $ url
  maybe (fail "could not parse the URL") (pure) $ useHttpsURI uri

toHero :: HeroResponse -> Hero
toHero response = Hero { heroName = (heroResponseName response)
                       , heroID   = HeroID $ unHeroID $ heroResponseID response
                       }
