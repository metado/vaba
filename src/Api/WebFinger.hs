{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.WebFinger where

import qualified Data.Aeson as Json
import           GHC.Generics hiding (Meta)
import           Control.Monad.Except
import           Servant hiding (Link)
import           Data.String

import           Config

type WebFingerAPI = ".well-known" :> "webfinger" :> QueryParam "resource" String :> Get '[JSON] [Fingerprint]

data Account = Account { 
  name :: String
, domain :: String
} deriving (Eq)

instance Show Account where
  show Account { name=n, domain=d } = "acct:" ++ n ++ "@" ++ d
  

data Link = Link {
  rel :: String
, linkType :: String
, href :: String
} deriving (Eq, Show, Generic)

data Fingerprint = Fingerprint {
  subject :: Account
, links :: [Link]
} deriving (Eq, Show, Generic)

outboxAPI :: Proxy WebFingerAPI
outboxAPI = Proxy

outboxEndpoint :: Config -> Server WebFingerAPI
outboxEndpoint resource config = getPosts
  where getPosts :: Handler [Fingerprint]
        getPosts = liftIO $ undefined
