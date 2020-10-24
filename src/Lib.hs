{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}


module Lib where

import           Data.Aeson
import           Network.HTTP.Req
import           Conferer
import           GHC.Generics
import           Data.Text               hiding ( find )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BSL
import           System.FilePath                ( )
import           Data.Foldable                  ( toList )
import           Control.Monad.Catch
import qualified Network.HTTP.Client           as HTTPClient
import           Network.HTTP.Types.Status      ( notModified304 )
import           Text.URI
import           System.FilePath
import           System.Directory
import           Data.List                      ( find )
import           Network.HTTP.Types.Header

newtype HeroJsonURL = HeroJsonURL { unHeroJsonUrl :: Text} deriving (Generic)
newtype ETag = ETag { unETag :: BS.ByteString}

newtype HeroID = HeroID { unHeroID :: Int } deriving (Show)

data Hero = Hero {
  heroName :: Text,
  heroID :: HeroID
} deriving (Show)

instance FromJSON Hero where
  parseJSON = withObject "Hero"
    $ \v -> Hero <$> v .: "localized_name" <*> (HeroID <$> v .: "id")

data JSONMapList a = JSONMapList [a] deriving (Show)

instance FromJSON a => FromJSON (JSONMapList a) where
  parseJSON = withObject "hash map"
    $ \obj -> JSONMapList <$> (mapM parseJSON $ toList obj)


getHerosRaw
  :: ETag -> Url 'Https -> Option 'Https -> Req (Maybe ETag, BS.ByteString)
getHerosRaw etag heroJsonUrl option = reqBr
  GET
  heroJsonUrl
  NoReqBody
  (option <> header "If-None-Match" (unETag etag))
  withEtag
 where
  withEtag
    :: HTTPClient.Response HTTPClient.BodyReader
    -> IO (Maybe ETag, BS.ByteString)
  withEtag response = do
    let etagMb = snd <$> find (\(headerName, _) -> headerName == hETag)
                              (HTTPClient.responseHeaders response)
    body' <- HTTPClient.brConsume $ HTTPClient.responseBody response
    let body = BS.concat body'
    pure (ETag <$> etagMb, body)

heroesETagFile :: FilePath -> FilePath
heroesETagFile home = home </> "heroes.etag"

heroesJSONFile :: FilePath -> FilePath
heroesJSONFile home = home </> "heroes.json"

-- TODO: Win `req` 3.7, Req is instance of MonadThrow, that can easily `catchIf` without going though IO.
-- Whenever LTS is up to date with 3.7 and GHCIDE works with it, update
getHerosWithCache
  :: FilePath -> Url 'Https -> Option 'Https -> IO (JSONMapList Hero)
getHerosWithCache home url option = do
  etag <- ETag <$> BS.readFile (heroesETagFile home) `catch` \(_ :: IOError) ->
    pure mempty
  heroesJson <- catchJust cacheHit (get etag) (readCache)
  maybe (fail "cant decode heroes json") pure $ decode $ BSL.fromStrict
    heroesJson
 where
  readCache :: a -> IO BS.ByteString
  readCache _ = BS.readFile (heroesJSONFile home)
  get :: ETag -> IO BS.ByteString
  get etag = do
    (etagMb, heroes) <- runReq defaultHttpConfig $ getHerosRaw etag url option
    let dir = takeDirectory (heroesJSONFile home)
    _ <- createDirectoryIfMissing True dir
    _ <- BS.writeFile (heroesJSONFile home) heroes
    _ <- maybe (pure ())
               (\e -> BS.writeFile (heroesETagFile home) (unETag e))
               etagMb
    pure heroes
  cacheHit :: HttpException -> Maybe ()
  cacheHit (VanillaHttpException (HTTPClient.HttpExceptionRequest _ (HTTPClient.StatusCodeException res _)))
    = if HTTPClient.responseStatus res == notModified304
      then Just ()
      else Nothing
  cacheHit _ = Nothing

data AppConfig = AppConfig
  { appConfigHome :: FilePath
  , appConfigHeroJsonURL :: HeroJsonURL
  } deriving (Generic)

instance Conferer.FromConfig AppConfig
instance Conferer.FromConfig HeroJsonURL

instance Conferer.DefaultConfig AppConfig where
  configDef = AppConfig
    { appConfigHome        = "tryhard"
    , appConfigHeroJsonURL =
      HeroJsonURL
        "https://raw.githubusercontent.com/odota/dotaconstants/master/build/heroes.json"
    }


someFunc :: IO ()
someFunc = do
  config        <- defaultConfig "tryhard"
  appConfig     <- getFromRootConfig config
  (url, option) <- prepareUrl appConfig
  home          <- transformPath $ appConfigHome appConfig
  heros         <- getHerosWithCache home url option
  putStrLn $ show heros


-- Examples
-- foo      -> /root/.foo/
-- /foo     -> /foo
-- foo/bar  -> /root/.foo/bar/
transformPath :: FilePath -> IO FilePath
transformPath og = if isValid valid
  then addTrailingPathSeparator <$> toAbsolute valid
  else fail "Home path is not valid"
 where
  valid = makeValid og
  toAbsolute path =
    if isAbsolute path then pure path else getAppUserDataDirectory path

prepareUrl
  :: (MonadFail m, MonadThrow m) => AppConfig -> m (Url 'Https, Option scheme)
prepareUrl config = do
  uri <- mkURI $ unHeroJsonUrl $ appConfigHeroJsonURL $ config
  url <- maybe (fail "could not parse the Heroes URL") (pure) $ useHttpsURI uri
  pure url
