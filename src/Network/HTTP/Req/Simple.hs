{-# LANGUAGE DataKinds #-}

module Network.HTTP.Req.Simple
  ( module Network.HTTP.Req
  , module Network.HTTP.Req.Simple
  )
where

import           Network.HTTP.Req

type URL = (Url 'Https, Option 'Https)
