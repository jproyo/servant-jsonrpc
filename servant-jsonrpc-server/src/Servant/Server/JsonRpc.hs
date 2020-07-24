module Servant.Server.JsonRpc
    ( JsonRpc
    , Request (..)
    , Response (..)
    , JsonRpcErr (..)
    ) where


import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Proxy
import           Protolude
import           Servant.JsonRpc
import           Servant.Server  (HasServer (..))

instance (FromJSON p, ToJSON e, ToJSON r)
    => HasServer (JsonRpc method p e r) context where

    type ServerT (JsonRpc method p e r) m = p -> m (Either (JsonRpcErr e) r)

    route _ cx = route (Proxy @(JsonRpcEndpoint p e r)) cx . fmap f
        where
        f x (Request _ p ix) = g ix <$> x p
        g ix (Right r) = Result ix r
        g _ (Left e)   = Errors e

    hoistServerWithContext _ _ f x p = f $ x p
