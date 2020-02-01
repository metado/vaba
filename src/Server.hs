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
import Config

type PostAPI = "notes" :> Get '[JSON] (IntHeaders [D.Post])
          :<|> "notes" :> Capture "id" String :> Get '[JSON] (Maybe D.Post)
          :<|> "notes" :> ReqBody '[JSON] D.Post :> Post '[JSON] D.HelloMessage

type Aux = "static" :> Raw

type AppAPI = PostAPI :<|> Aux

data SortBy = PubDate | Title deriving (Eq, Show)

noteAPI :: Proxy PostAPI
noteAPI = Proxy

appAPI :: Proxy AppAPI
appAPI = Proxy

-- | Custom headers, reminder
type IntHeaders a = (Headers '[Header "X-An-Int" Int] a)

-- | Construct a value with list of custom headers
addIntHeader :: Int -> a -> IntHeaders a
addIntHeader i a = addHeader i a

noteServer :: Config -> Server PostAPI
noteServer config = getPosts :<|> getPost :<|> postPost
  where getPosts :: Handler (Headers '[Header "X-An-Int" Int] [D.Post])
        getPosts = do
          notes <- readPosts
          return $ addIntHeader 1797 notes

        getPost :: String -> Handler (Maybe D.Post)
        getPost id = undefined

        postPost :: D.Post -> Handler D.HelloMessage
        postPost note = do 
          _ <- liftIO $ putStrLn "Hello!"
          return $ D.HelloMessage { D.msg = "Thanks!" }

        readPosts = undefined

server :: Config -> Server AppAPI
server config = (noteServer config) :<|> (serveDirectoryWebApp $ staticDir config)

app :: Config -> IO Application
app config = return $ serve appAPI $ server config
