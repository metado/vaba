{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server (PostAPI, noteAPI, app) where


import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Attoparsec.ByteString
import           Data.ByteString (ByteString)
import           Data.String.Conversions
import           Data.Time.Calendar
import           GHC.Generics
import           Network.HTTP.Media ((//), (/:))
import           Network.Wai.Handler.Warp
import           Servant
import           System.Directory
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

import qualified Data as D
import qualified Database as DB
import           Config
import           Feed

type PostAPI = "posts" :> Get '[JSON] [D.Post]
          :<|> "posts" :> ReqBody '[JSON] D.Post :> Post '[JSON] D.ResultMessage

type Aux = "static" :> Raw

type AppAPI = PostAPI :<|> Aux :<|> FeedAPI

data SortBy = PubDate | Title deriving (Eq, Show)

noteAPI :: Proxy PostAPI
noteAPI = Proxy

appAPI :: Proxy AppAPI
appAPI = Proxy

noteServer :: Config -> Server PostAPI
noteServer config = getPosts :<|> postPost
  where getPosts :: Handler [D.Post]
        getPosts = liftIO $ DB.getPosts config

        postPost :: D.Post -> Handler D.ResultMessage
        postPost note = do 
          liftIO $ DB.addPost config note
          return $ D.ResultMessage { D.msg = "Thanks!" }

server :: Config -> Server AppAPI
server config = (noteServer config) :<|> (serveDirectoryWebApp $ staticDir config) :<|> (feedServer config)

app :: Config -> IO Application
app config = return $ serve appAPI $ server config
