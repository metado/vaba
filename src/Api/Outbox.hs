{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Outbox where

import qualified Data.Aeson as Json
import           GHC.Generics hiding (Meta)
import           Control.Monad.Except
import           Servant
import           Data.String

import qualified Data as D
import qualified Database as DB
import           Config
import           Api.Feed

data PublicKey = PublicKey {
  keyId :: String
, keyOwner :: String
, publicKeyPem :: String            -- TODO: Use http://hackage.haskell.org/package/cryptonite-0.26/docs/Crypto-PubKey-RSA.html
} deriving (Eq, Show, Generic)

data Actor = Actor {
  actorId :: String
, actorType :: String
, preferredUserName :: String
, inbox :: String
, publicKey :: PublicKey
} deriving (Eq, Show, Generic)

newtype ActorAddress = ActorAddress { 
  getAddress :: String 
} deriving (Eq, Show, Generic, IsString)

-- ^ https://www.w3.org/ns/activitystreams#Object
data ObjectMeta = ObjectMeta {
  attributedTo :: String
, name :: String
, content :: String
, inReplyTo :: String
, to :: [String]
} deriving (Eq, Show, Generic)

instance Json.FromJSON ObjectMeta where
  parseJSON (Json.Object v) = ObjectMeta <$> v Json..: "x" <*> v Json..: "x" <*> v Json..: "x" <*> v Json..: "x" <*> v Json..: "x"




-- ^ https://www.w3.org/ns/activitystreams#Note
data Object = Note Meta ObjectMeta | Place Meta ObjectMeta PlaceMeta deriving (Eq, Show, Generic)

data PlaceMeta = PlaceMeta { 
  accuracy :: Float 
, latitude :: Float
, longitude :: Float
} deriving (Eq, Show, Generic)

data ActivityMeta = ActivityMeta {
  actor :: String
, object :: String
, target :: String
, origin :: String
} deriving (Eq, Show, Generic)

data Meta = Meta { 
  metaId :: Maybe String
, metaType :: Maybe String
} deriving (Eq, Show, Generic)

data Activity = Follow ActivityMeta | Create ActivityMeta deriving (Eq, Show, Generic)

newtype Response = Response { msg :: String } deriving (Eq, Show, Generic)

type OutboxAPI = "outbox" :> Get '[JSON] [D.Post]                                   -- Server-to-server
            :<|> "outbox" :> ReqBody '[JSON] Object :> Post '[JSON] Response -- Client-to-server

outboxAPI :: Proxy OutboxAPI
outboxAPI = Proxy

outboxEndpoint :: Config -> Server OutboxAPI
outboxEndpoint config = getPosts :<|> postPost
  where getPosts :: Handler [D.Post]
        getPosts = liftIO $ DB.getPosts config

        postPost :: Object -> Handler Response
        postPost entity = undefined
