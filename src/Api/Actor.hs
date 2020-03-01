{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Actor where

import           GHC.Generics

import           Servant (Capture, Get, JSON, Proxy(..), Server, (:>))
import           Data.Aeson
import           Data.Text

import           Config

type URL = Text

data PublicKey = PublicKey {
  publicKeyId :: Text            -- ^ Key's id, in JSON gets concatenated with `keyOwner`
, publicKeyOwner :: URL          -- ^ Actor id
, publicKeyPem :: Text           -- ^ Key content itself (with headers). Key is not shortened when serialized
} deriving (Eq, Show, Generic)

data Actor = Actor {
  actorId :: URL                 -- ^ Self-reference
, actorType :: Text              -- ^ Usually "Person"
, actorPreferredUsername :: Text -- ^ Chosen by user
, actorInbox :: URL              -- ^ Where messages shoudld be sent
, actorPublicKey :: PublicKey    -- ^ Actor's public RSA key
} deriving (Eq, Show, Generic) 


type ActorAPI = "users" :> Capture "userName" Text :> Get '[JSON] Text

actorAPI :: Proxy ActorAPI
actorAPI = Proxy

actor :: Config -> Server ActorAPI
actor config = undefined

