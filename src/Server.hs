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


import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

import qualified Data as D
import qualified Database as DB
import Config

type PostAPI = "posts" :> Get '[JSON] [D.Post]
          :<|> "posts" :> ReqBody '[JSON] D.Post :> Post '[JSON] D.HelloMessage

type Aux = "static" :> Raw

type AppAPI = PostAPI :<|> Aux

data SortBy = PubDate | Title deriving (Eq, Show)

noteAPI :: Proxy PostAPI
noteAPI = Proxy

appAPI :: Proxy AppAPI
appAPI = Proxy

noteServer :: Config -> Server PostAPI
noteServer config = getPosts :<|> postPost
  where getPosts :: Handler [D.Post]
        getPosts = do
          liftIO $ DB.getPosts config

        postPost :: D.Post -> Handler D.HelloMessage
        postPost note = do 
          _ <- liftIO $ DB.addPost config note
          return $ D.HelloMessage { D.msg = "Thanks!" }

server :: Config -> Server AppAPI
server config = (noteServer config) :<|> (serveDirectoryWebApp $ staticDir config)

app :: Config -> IO Application
app config = return $ serve appAPI $ server config
