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
module Servant.Yaml (YAML) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Yaml          (FromJSON, ToJSON, decodeEither, encode)
import Servant.API        (Accept (..), MimeRender (..), MimeUnrender (..))

import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Media   as M

data YAML -- deriving Typeable

yamlMime, yamlMime1, yamlMime2, yamlMime3 :: M.MediaType
yamlMime = "application" M.// "x-yaml"
yamlMime1 = "text" M.// "yaml"
yamlMime2 = "text" M.// "x-yaml"
yamlMime3 = "text" M.// "vnd.yaml"

-- | @application/x-yaml@
instance Accept YAML where
    contentType _ = yamlMime
    contentTypes _ = yamlMime :| [ yamlMime1, yamlMime2, yamlMime3 ]

-- | `encode`
instance ToJSON a => MimeRender YAML a where
    mimeRender _ = LBS.fromStrict . encode

-- | `decodeEither`
instance FromJSON a => MimeUnrender YAML a where
    mimeUnrender _ = decodeEither . LBS.toStrict
