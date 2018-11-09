{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- |
-- Module      :  Servant.Yaml
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- An @YAML@ empty data type with `MimeRender` instances for @yaml@ /
-- @aeson@'s `ToJSON` class and `Value` datatype.  You should only need to
-- import this module for it's instances and the `YAML` datatype.:
--
-- >>> type YamlGET a = Get '[YAML] a
--
-- Will then check that @a@ has a `ToJSON` instance (`Value` has).
module Servant.Yaml where

import           Data.Yaml
                 (FromJSON, ToJSON, decodeEither', encode,
                 prettyPrintParseException)
import           Servant.API
                 (Accept (..), MimeRender (..), MimeUnrender (..))

import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Media   as M

data YAML

-- | @application/x-yaml@
instance Accept YAML where
    contentType _ = "application" M.// "x-yaml"

-- | `encode`
instance ToJSON a => MimeRender YAML a where
    mimeRender _ = LBS.fromStrict . encode

-- | `decodeEither`
instance FromJSON a => MimeUnrender YAML a where
    mimeUnrender _ = either (Left . prettyPrintParseException) Right . decodeEither' . LBS.toStrict
