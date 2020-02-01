{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server (NoteAPI, noteAPI, app) where


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

import Data
import Config

type NoteAPI = "notes" :> Get '[JSON] (IntHeaders [Note])
          :<|> "notes" :> Capture "id" String :> Get '[JSON] (Maybe Note)
          :<|> "notes" :> ReqBody '[JSON] Note :> Post '[JSON] HelloMessage

type Aux = "static" :> Raw

type AppAPI = NoteAPI :<|> Aux

data SortBy = PubDate | Title deriving (Eq, Show)

noteAPI :: Proxy NoteAPI
noteAPI = Proxy

appAPI :: Proxy AppAPI
appAPI = Proxy

-- | Custom headers, reminder
type IntHeaders a = (Headers '[Header "X-An-Int" Int] a)

-- | Construct a value with list of custom headers
addIntHeader :: Int -> a -> IntHeaders a
addIntHeader i a = addHeader i a

noteServer :: Config -> Server NoteAPI
noteServer config = getNotes :<|> getNote :<|> postNote
  where getNotes :: Handler (Headers '[Header "X-An-Int" Int] [Note])
        getNotes = do
          notes <- readNotes
          return $ addIntHeader 1797 notes

        getNote :: String -> Handler (Maybe Note)
        getNote id = do
          notes <- readNotes
          return $ find (\n -> noteId n == id) notes 

        postNote :: Note -> Handler HelloMessage
        postNote note = do 
          _ <- liftIO $ putStrLn "Hello!"
          return $ HelloMessage { msg = "Thanks!" }

        readNotes :: Handler [Note]
        readNotes = liftIO $ listNotes $ notesDir config

server :: Config -> Server AppAPI
server config = (noteServer config) :<|> serveDirectoryWebApp "/Users/antonparkhomenko/workspace/sandbox/dovlatov/elm-src"

app :: Config -> IO Application
app config = return $ serve appAPI $ server config
