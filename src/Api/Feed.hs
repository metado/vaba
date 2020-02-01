{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Api.Feed (FeedAPI, feedServer) where

import           Control.Monad.Except
import           Servant

import           Config
import qualified Data       as D
import qualified Client     as C


type FeedAPI = "feed" :> Get '[JSON] [D.Post]

feedServer :: Config -> Server FeedAPI
feedServer config = getFeed
  where getFeed :: Handler [D.Post]
        getFeed = liftIO $ C.feed config
