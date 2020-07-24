{-# LANGUAGE AllowAmbiguousTypes #-}
-- |
-- Module: Servant.JsonRpc
module Servant.JsonRpc
    ( JsonRpc
    , JsonRpcEndpoint
    , Request (..)
    , Response (..)
    , JsonRpcErr (..)
    ) where


import           Control.Applicative (liftA3)
import           Control.Monad.Fail
import           Data.Aeson          (FromJSON (..), ToJSON (..), Value (Null), object, withObject, (.:), (.:?), (.=))
import           Data.Aeson.Types    (Parser)
import           Data.Maybe
import           Data.Word           (Word64)
import           Fcf
import           GHC.Base            (String)
import           GHC.TypeLits        (Symbol)
import           Protolude
import           Servant.API         ((:>), JSON, Post, ReqBody)


data Request p
    = Request { method :: Text, params :: p, id :: Word64 }
    deriving (Eq, Show)

instance ToJSON p => ToJSON (Request p) where
    toJSON (Request m p ix) =
        object [ "jsonrpc" .= ("2.0" :: Text)
               , "method" .= m
               , "params" .= p
               , "id" .= ix ]

instance FromJSON p => FromJSON (Request p) where
    parseJSON = withObject "JsonRpc Request" $ \obj -> do
        ix <- obj .: "id"
        method <- obj .: "method"
        p <- obj .: "params"
        version <- obj .: "jsonrpc"

        versionGuard version . pure $ Request method p ix


versionGuard :: Text -> Parser a -> Parser a
versionGuard v x
    | v == "2.0" = x
    | otherwise  = fail "unknown version"


data Response e r
    = Result Word64 r
    | Errors (JsonRpcErr e)
    deriving (Eq, Show)


data JsonRpcErr e = JsonRpcErr Int Text (Maybe e)
    deriving (Eq, Show)

instance (FromJSON e, FromJSON r) => FromJSON (Response e r) where
    parseJSON = withObject "Response" $ \obj -> do
        ix <- obj .: "id"
        version <- obj .: "jsonrpc"
        result <- obj .:? "result"
        err <- obj .:? "error"
        versionGuard version $ pack ix result err

        where

        pack (Just ix) (Just r) Nothing = pure $ Result ix r
        pack Nothing Nothing (Just e)   = Errors <$> parseErr e
        pack _ _ _                      = fail "invalid response"

        parseErr = withObject "Error" $
            liftA3 JsonRpcErr <$> (.: "code") <*> (.: "message") <*> (.:? "data")

instance (ToJSON e, ToJSON r) => ToJSON (Response e r) where
    toJSON (Result ix r) =
        object [ "jsonrpc" .= ("2.0" :: Text)
               , "result" .= r
               , "id" .= ix
               ]

    toJSON (Errors (JsonRpcErr c msg err)) =
        object [ "jsonrpc" .= ("2.0" :: Text)
               , "id" .= Null
               , "error" .=
                    object [ "code" .= c
                           , "message" .= msg
                           , "data" .= err
                           ]
               ]


-- | This is the type used to specify JSON-RPC endpoints
data JsonRpc (method :: Symbol) p e r

type JsonRpcEndpoint p e r
    = ReqBody '[JSON] (Request p) :> Post '[JSON] (Response e r)


infixr 4 <++>
infixl 5 `And`

data And :: (operation, resolver) -> (operation, resolver) -> Exp [(operation, resolver)]
type instance Eval (And newSolver solvers) = '[newSolver, solvers]

data (<++>) :: (operation, resolver) -> Exp [(operation, resolver)] -> Exp [(operation,resolver)]
type instance Eval ((<++>) newOper others) = newOper ': (Eval others)

type JsonRpcMap operation solver = '(operation,solver)


data SomeADT = One | Two | Three | Four | Five
  deriving (Enum, Ord, Eq, Show, Bounded, Read)

type ToEval = (JsonRpcMap 'One 'True)
         <++> (JsonRpcMap 'Two 'False)
         <++> (JsonRpcMap 'Three 'True)
         <++> (JsonRpcMap 'Four 'False)
        `And` (JsonRpcMap 'Five 'True)


type LookInto toEval = Eval (FromMaybe 'True (Eval (Lookup toEval (Eval ToEval))))

getADT :: String -> SomeADT
getADT = fromJust . readMaybe

solveLook :: forall (t :: SomeADT). Proxy t -> Bool
solveLook _ = algo @(LookInto t)

class Algo b where
  algo :: Bool

instance Algo 'True where algo = True
instance Algo 'False where algo = False
