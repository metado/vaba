{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Feed (FeedAPI, feedAPI) where

import Prelude.Compat

import Control.Monad.Except
import Servant

import qualified Data as D
import qualified Database as DB
import Config

type FeedAPI = "feed" :> Get '[JSON] [D.Post]

feedAPI :: Proxy FeedAPI
feedAPI = Proxy

feedServer :: Config -> Server FeedAPI
feedServer config = getFeed
  where getFeed :: Handler [D.Post]
        getFeed = liftIO $ DB.getPosts config
