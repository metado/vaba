{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Actor (ActorAPI, actor) where

import           GHC.Generics

import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.TH as ATH
import           Data.Text
import           Servant (Capture, Get, Handler, JSON, Proxy(..), Server, err404, (:>))

import           Data hiding (Actor)
import           Config

data PublicKey = PublicKey {
  publicKeyId :: Text            -- ^ Key's id, in JSON gets concatenated with `keyOwner`
, publicKeyOwner :: Text         -- ^ Actor id
, publicKeyPem :: Text           -- ^ Key content itself (with headers). Key is not shortened when serialized
} deriving (Eq, Show, Generic)

ATH.deriveJSON ATH.defaultOptions ''PublicKey

-- | Fake PEM
pem = ""

data Actor = Actor {
  actorId :: Text                -- ^ Self-reference, required
, actorType :: Text              -- ^ Usually "Person", required
, actorPreferredUsername :: Text -- ^ Chosen by user
, actorInbox :: Text             -- ^ Where messages shoudld be sent
, actorPublicKey :: PublicKey    -- ^ Actor's public RSA key
} deriving (Eq, Show, Generic) 

ATH.deriveJSON ATH.defaultOptions ''Actor

buildActor :: Config -> IO Actor
buildActor config = pure actor
  where actor = Actor id "Person" preferredName inbox key
        key = PublicKey "main-key" id pem
        id = (accountUrl . ownAccount) config
        preferredName = name config
        inbox = (inboxUrl . ownAccount) config

type ActorAPI = "users" :> Capture "userName" Text :> Get '[JSON] Actor

actorAPI :: Proxy ActorAPI
actorAPI = Proxy

actor :: Config -> Server ActorAPI
actor config = actorHandler
  where actorHandler :: Text -> Handler Actor
        actorHandler userName = if (matching userName) then liftIO $ buildActor config else throwError err404
        matching name = ownAccount config == Account name ((accountDomain . ownAccount) config)

