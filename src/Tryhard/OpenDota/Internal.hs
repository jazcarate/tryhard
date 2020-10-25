{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Tryhard.OpenDota.Internal where

import           Data.Aeson
import           Network.HTTP.Req
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BSL
import           System.FilePath                ( )
import           Data.Foldable                  ( toList )
import           Control.Monad.Catch
import qualified Network.HTTP.Client           as HTTPClient
import           Network.HTTP.Types.Status      ( notModified304 )
import           System.FilePath
import           System.Directory
import           Data.List                      ( find )
import           Network.HTTP.Types.Header

newtype HeroIDResponse = HeroIDResponse { unHeroID :: Int } deriving (Eq)

data HeroResponse = HeroResponse {
  heroResponseName :: Text,
  heroResponseID :: HeroIDResponse
}

data HeroMatchupResponse = HeroMatchupResponse {
  heroMatchupResponseHeroID :: HeroIDResponse,
  heroMatchupGamesResponsePlayed :: Int,
  heroMatchupResponseWins :: Int
}

newtype ETag = ETag { unETag :: BS.ByteString}

instance FromJSON HeroMatchupResponse where
  parseJSON = withObject "hero matchup" $ \v ->
    HeroMatchupResponse
      <$> (HeroIDResponse <$> v .: "hero_id")
      <*> (v .: "games_played")
      <*> (v .: "wins")

instance FromJSON HeroResponse where
  parseJSON = withObject "hero" $ \v ->
    HeroResponse <$> (v .: "localized_name") <*> (HeroIDResponse <$> v .: "id")

newtype JSONMapList a = JSONMapList { unList ::  [a] }

instance FromJSON a => FromJSON (JSONMapList a) where
  parseJSON = withObject "hash map"
    $ \obj -> JSONMapList <$> (mapM parseJSON $ toList obj)


getHeroMatchupRaw :: Url 'Https -> Option 'Https -> Req [HeroMatchupResponse]
getHeroMatchupRaw heroMatchupUrl option =
  responseBody <$> req GET heroMatchupUrl NoReqBody jsonResponse option

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

-- TODO: Win `req` 3.7, Req is instance of MonadThrow, that can easily `catchIf` without going though IO.
-- Whenever LTS is up to date with 3.7 and GHCIDE works with it, update
getHerosWithCache
  :: FilePath -> FilePath -> (Url 'Https, Option 'Https) -> IO [HeroResponse]
getHerosWithCache etagFilePath heroesFilePath (url, option) = do
  etag <- ETag <$> BS.readFile etagFilePath `catch` \(_ :: IOError) ->
    pure mempty
  heroesJson <- catchJust cacheHit (get etag) (readCache)
  heroes :: (JSONMapList HeroResponse) <-
    maybe (fail "cant decode heroes json") pure $ decode $ BSL.fromStrict
      heroesJson
  pure $ unList heroes
 where
  readCache :: a -> IO BS.ByteString
  readCache _ = BS.readFile heroesFilePath
  get :: ETag -> IO BS.ByteString
  get etag = do
    (etagMb, heroes) <- runReq defaultHttpConfig $ getHerosRaw etag url option
    let dir = takeDirectory heroesFilePath
    _ <- createDirectoryIfMissing True dir
    _ <- BS.writeFile heroesFilePath heroes
    _ <- maybe (pure ()) (\e -> BS.writeFile etagFilePath (unETag e)) etagMb
    pure heroes
  cacheHit :: HttpException -> Maybe ()
  cacheHit (VanillaHttpException (HTTPClient.HttpExceptionRequest _ (HTTPClient.StatusCodeException res _)))
    = if HTTPClient.responseStatus res == notModified304
      then Just ()
      else Nothing
  cacheHit _ = Nothing


getHeroMatchupInt
  :: Int
  -> (Url 'Https, Option 'Https)
  -> IO ([HeroMatchupResponse])
getHeroMatchupInt heroId (baseUrl, option) =
  runReq defaultHttpConfig $ getHeroMatchupRaw url option
 where
  url = baseUrl /: "heroes" /: (pack $ show heroId) /: "matchups"
