{-# LANGUAGE DeriveGeneric #-}

module Tryhard.Config where

import qualified Conferer
import           GHC.Generics                   ( Generic )
import           Data.Text                      ( Text )
import           System.FilePath                ( isValid
                                                , addTrailingPathSeparator
                                                , makeValid
                                                , isAbsolute
                                                )
import           System.Directory               ( getAppUserDataDirectory )
import           Control.Monad.Catch            ( MonadThrow )
import           Network.HTTP.Req.Simple        ( URL
                                                , useHttpsURI
                                                )
import           Text.URI                       ( mkURI )

newtype UserDataPath = UserDataPath { unPath :: FilePath } deriving (Generic)
newtype RawURL = RawURL { unUrl :: Text } deriving (Generic)

data AppConfig = AppConfig
  { appConfigHome :: UserDataPath
  , appConfigHeroJsonURL :: RawURL
  , appConfigOpenDotaApi :: RawURL
  } deriving (Generic)

instance Conferer.FromConfig AppConfig
instance Conferer.FromConfig UserDataPath
instance Conferer.FromConfig RawURL

instance Conferer.DefaultConfig AppConfig where
  configDef = AppConfig
    { appConfigHome        = UserDataPath "tryhard"
    , appConfigHeroJsonURL =
      RawURL
        "https://raw.githubusercontent.com/odota/dotaconstants/master/build/heroes.json"
    , appConfigOpenDotaApi = RawURL "https://api.opendota.com/api/"
    }

toPath :: UserDataPath -> IO FilePath
toPath = transformPath . unPath

transformPath :: FilePath -> IO FilePath
transformPath og = if isValid valid
  then addTrailingPathSeparator <$> toAbsolute valid
  else fail "Home path is not valid"
 where
  valid = makeValid og
  toAbsolute path =
    if isAbsolute path then pure path else getAppUserDataDirectory path


prepareUrl :: (MonadFail m, MonadThrow m) => RawURL -> m URL
prepareUrl RawURL { unUrl = url } = do
  uri <- mkURI $ url
  maybe (fail "could not parse the URL") (pure) $ useHttpsURI uri
