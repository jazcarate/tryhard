{-# LANGUAGE ScopedTypeVariables #-}
module Tryhard.OpenDota.Internal where

import           Data.Aeson
import           Network.HTTP.Req.Simple
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BSL
import           Data.Foldable                  ( toList )
import           Control.Monad.Catch            ( catchJust
                                                , catch
                                                )
import qualified Network.HTTP.Client           as HTTPClient
import           Network.HTTP.Types.Status      ( notModified304 )
import           System.FilePath                ( takeDirectory )
import           System.Directory               ( createDirectoryIfMissing )
import           Data.List                      ( find )
import           Network.HTTP.Types.Header      ( hETag )
import           Data.Functor.Compose           ( Compose(Compose)
                                                , getCompose
                                                )


newtype HeroIDResponse = HeroIDResponse { unHeroID :: Int } deriving (Eq, Show)

data HeroResponse = HeroResponse
  { heroResponseName :: Text
  , heroResponseID :: HeroIDResponse
  , heroResponseLegs :: Int
  }

data HeroMatchupResponse = HeroMatchupResponse
  { heroMatchupResponseHeroID :: HeroIDResponse
  , heroMatchupGamesResponsePlayed :: Int
  , heroMatchupResponseWins :: Int
  } deriving (Show)

data HeroComboResponse = HeroComboResponse
  { heroComboResponseTeamA :: [HeroIDResponse]
  , heroComboResponseTeamB :: [HeroIDResponse]
  } deriving (Show)

newtype ETag = ETag { unETag :: BS.ByteString}

instance FromJSON HeroMatchupResponse where
  parseJSON = withObject "hero matchup" $ \v ->
    HeroMatchupResponse
      <$> (HeroIDResponse <$> v .: "hero_id")
      <*> (v .: "games_played")
      <*> (v .: "wins")

instance FromJSON HeroResponse where
  parseJSON = withObject "hero" $ \v ->
    HeroResponse
      <$> (v .: "localized_name")
      <*> (HeroIDResponse <$> v .: "id")
      <*> (v .: "legs")

instance FromJSON HeroComboResponse where
  parseJSON = withObject "hero combo" $ \v ->
    HeroComboResponse
      <$> (getCompose $ HeroIDResponse <$> Compose (v .: "teama"))
      <*> (getCompose $ HeroIDResponse <$> Compose (v .: "teamb"))

newtype JSONMapList a = JSONMapList { unList ::  [a] }

instance FromJSON a => FromJSON (JSONMapList a) where
  parseJSON = withObject "hash map"
    $ \obj -> JSONMapList <$> (mapM parseJSON $ toList obj)

getHeroMatchupRaw :: URL -> Req [HeroMatchupResponse]
getHeroMatchupRaw = simpleGetList

getHeroComboRaw :: URL -> Req [HeroComboResponse]
getHeroComboRaw = simpleGetList

simpleGetList :: FromJSON a => URL -> Req [a]
simpleGetList (url, option) =
  responseBody <$> req GET url NoReqBody jsonResponse option

getHerosRaw :: ETag -> URL -> Req (Maybe ETag, BS.ByteString)
getHerosRaw etag (heroJsonUrl, option) = reqBr
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

getHeroes :: FilePath -> FilePath -> URL -> IO [HeroResponse]
getHeroes etagFilePath heroesFilePath url = do
  etag <- ETag <$> BS.readFile etagFilePath `catch` \(_ :: IOError) ->
    pure mempty
  heroesJson <- catchJust cacheHit (get etag) (readCache)
  responses  <- either fail pure $ eitherDecode $ BSL.fromStrict heroesJson
  pure $ unList $ responses
 where
  readCache :: a -> IO BS.ByteString
  readCache _ = BS.readFile heroesFilePath
  get :: ETag -> IO BS.ByteString
  get etag = do
    (etagMb, heroes) <- runReq defaultHttpConfig $ getHerosRaw etag url
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


getHeroMatchup :: Int -> URL -> IO ([HeroMatchupResponse])
getHeroMatchup heroId (baseUrl, option) = runReq defaultHttpConfig
  $ getHeroMatchupRaw (url, option)
  where url = baseUrl /: "heroes" /: (pack $ show heroId) /: "matchups"


getHerosCombo :: [Int] -> [Int] -> URL -> IO ([HeroComboResponse])
getHerosCombo heroIdTeamA heroIdTeamB (baseUrl, baseOptions) = do
  responses <- runReq defaultHttpConfig $ getHeroComboRaw (url, option)
  pure $ (tidyTeams heroIdTeamA) <$> responses
 where
  option = baseOptions <> query
  query =
    mconcat $ map ((=:) "teamA") heroIdTeamA <> map ((=:) "teamB") heroIdTeamB
  url = baseUrl /: "findMatches"

-- This way "team A" is alyways tam A that was sent
tidyTeams :: [Int] -> HeroComboResponse -> HeroComboResponse
tidyTeams heroes (HeroComboResponse a b) = HeroComboResponse a' b'
 where
  (a', b') = if (unHeroID <$> a) `included` heroes then (a, b) else (b, a)

included :: Eq a => [a] -> [a] -> Bool
included needle haystack = any id $ (==) <$> needle <*> haystack
