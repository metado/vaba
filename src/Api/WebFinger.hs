{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Api.WebFinger (WebFingerAPI, webFinger) where

import           Control.Monad.Except
import qualified Data.Text as T
import           GHC.Generics hiding (Meta)
import           Servant

import           Config
import           Data (Account(..), ownAccount, Fingerprint(..), Rel(..))

type WebFingerAPI = ".well-known" :> "webfinger" :> QueryParam "resource" Account :> Get '[JSON] Fingerprint

webFingerAPI :: Proxy WebFingerAPI
webFingerAPI = Proxy

webFinger :: Config -> Server WebFingerAPI
webFinger config (Just account) = response
  where getPosts :: Handler Fingerprint
        getPosts = liftIO $ ownFinger <$ (putStrLn $ "requesting " ++ show account)
        response = if matching then getPosts else throwError err404
        matching = subject ownFinger == account
        ownFinger = getOwnFinger config
webFinger _ Nothing = throwError err404

getOwnFinger :: Config -> Fingerprint
getOwnFinger config = Fingerprint { subject = ownAccount config, links = [rel] } 
  where rel = Self "application/activity+json" (ownEndpoint config)
