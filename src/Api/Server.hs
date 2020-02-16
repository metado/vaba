{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Server (app) where

import           Servant

import qualified Data       as D
import qualified Database   as DB
import           Config (Config (..))
import           Api.Feed (FeedAPI, feedServer)
import           Api.PostApi

type Aux = "static" :> Raw

type AppAPI = PostAPI :<|> Aux :<|> FeedAPI

appAPI :: Proxy AppAPI
appAPI = Proxy

server :: Config -> Server AppAPI
server config = (postServer config) 
                :<|> (serveDirectoryWebApp $ staticDir config) 
                :<|> (feedServer config)

app :: Config -> IO Application
app config = return $ serve appAPI $ server config
