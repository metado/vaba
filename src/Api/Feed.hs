{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Feed (FeedAPI, feedServer) where

import           Control.Monad.Except (liftIO)
import           Servant

import           Config
import qualified Data       as D
import qualified Database   as DB
import           Client (feed)


type FeedAPI = "feed" :> Get '[JSON] [D.Post]

feedServer :: Config -> Server FeedAPI
feedServer config = getFeed
  where getFeed :: Handler [D.Post]
        getFeed = liftIO $ actors >>= feed

        actors :: IO [D.Actor]
        actors = DB.listActors config
