{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Api.WebFinger (WebFingerAPI, webFinger) where

import           Control.Monad.Except
import           Control.Lens.Setter (set)
import           Control.Lens.Type
import           Data.Aeson
import           Data.Aeson.Types
import           Data.String
import qualified Data.Text as T
import           GHC.Generics hiding (Meta)
import           Servant hiding (Link, URI, uriScheme)
import           Text.URI hiding (QueryParam)
import qualified Text.URI.Lens as UL
import qualified Text.URI.QQ as UQ

import           Config
import           Data (Account(..), ownAccount, Fingerprint(..), Rel(..))

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
  where socket = host config <> ":" <> (T.pack $ show (port config))
        endpoint = set UL.uriPath (ownPath config) (ownHost config)
        rel = Self "application/activity+json" endpoint

