{-# LANGUAGE DataKinds #-}

module Network.HTTP.Req.Simple
  ( module Network.HTTP.Req
  , URL
  )
where

import           Network.HTTP.Req

type URL = (Url 'Https, Option 'Https)
