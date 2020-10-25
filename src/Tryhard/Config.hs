{-# LANGUAGE DeriveGeneric #-}

module Tryhard.Config where

import           Conferer
import           GHC.Generics
import           Data.Text                      ( Text )
import           System.FilePath
import           System.Directory

newtype UserDataPath = UserDataPath { unPath :: FilePath } deriving (Generic)

data AppConfig = AppConfig
  { appConfigHome :: UserDataPath
  , appConfigHeroJsonURL :: Text
  , appConfigOpenDotaApi :: Text
  } deriving (Generic)

instance Conferer.FromConfig AppConfig
instance Conferer.FromConfig UserDataPath

instance Conferer.DefaultConfig AppConfig where
  configDef = AppConfig
    { appConfigHome        = UserDataPath "tryhard"
    , appConfigHeroJsonURL =
      "https://raw.githubusercontent.com/odota/dotaconstants/master/build/heroes.json"
    , appConfigOpenDotaApi = "https://api.opendota.com/api/"
    }



toPath :: UserDataPath -> IO FilePath
toPath = transformPath . unPath

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
