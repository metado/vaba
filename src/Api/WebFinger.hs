{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.WebFinger (WebFingerAPI, webFinger) where

import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.TH as ATH
import           Data.String
import qualified Data.Text as T
import           GHC.Generics hiding (Meta)
import           Servant hiding (Link)

import           Config
import           Data (Account(..), ownAccount)

type URL = String

profilePage :: T.Text
profilePage = "http://webfinger.net/rel/profile-page"

data Rel = ProfilePage String URL | Self String URL deriving (Eq, Show, Generic)

instance ToJSON Rel where
  toJSON (ProfilePage t u) = 
    object ["rel" .= profilePage, "type" .= t, "href" .= u]
  toJSON (Self t u) =
    object ["rel" .= self, "type" .= t, "href" .= u]
    where self :: T.Text
          self = "self"

instance FromJSON Rel where
  parseJSON = undefined

type WebFingerAPI = ".well-known" :> "webfinger" :> QueryParam "resource" Account :> Get '[JSON] Fingerprint

webFingerAPI :: Proxy WebFingerAPI
webFingerAPI = Proxy

webFinger :: Config -> Server WebFingerAPI
webFinger config (Just account) = response
  where getPosts :: Handler Fingerprint
        getPosts = liftIO $ getOwnFinger config <$ (putStrLn $ "requesting " ++ show account)
        response = if matching then getPosts else throwError err404
        matching = subject (getOwnFinger config) == account
webFinger _ Nothing = throwError err404

getOwnFinger :: Config -> Fingerprint
getOwnFinger config = Fingerprint { subject = ownAccount config, links = [rel] } 
  where socket = (host config) ++ ":" ++ show (port config)
        rel = Self "application/activity+json" $ "https://" ++ socket ++ "/users/" ++ name config


data Fingerprint = Fingerprint {
  subject :: Account
, links :: [Rel]
} deriving (Eq, Show, Generic)

ATH.deriveJSON ATH.defaultOptions ''Fingerprint

