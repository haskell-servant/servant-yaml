{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Main (main) where

import Prelude        ()
import Prelude.Compat

import Data.Aeson.TH
import Data.Char          (toLower)
import Data.Maybe         (fromMaybe)
import Network.Wai        (Application)
import Servant
import Servant.Yaml
import System.Environment (getArgs, lookupEnv)
import Text.Read          (readMaybe)

import qualified Network.Wai.Handler.Warp as Warp

data Foo = Foo
    { _fooFoo :: Int
    , _fooBar :: String
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 4} ''Foo)

type API = "foo" :> ReqBody '[JSON, YAML] Foo :> Post '[JSON, YAML] Foo
      :<|> Get '[YAML, JSON] Foo

api :: Proxy API
api = Proxy

server :: Server API
server = pure :<|>  pure (Foo 42 "Yaml!")

app :: Application
app = serve api server

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("run":_) -> do
            port <- fromMaybe 8000 . (>>= readMaybe) <$> lookupEnv "PORT"
            putStrLn $ "http://localhost:" ++ show port ++ "/"
            Warp.run port app
        _ -> do
            putStrLn "Example application, used as a compilation check"
            putStrLn "To run, pass run argument: --test-arguments run"
