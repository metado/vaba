{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Feed (FeedAPI, feedAPI, feedServer) where

import           Prelude.Compat

import           Control.Monad.Except
import           Servant

import           Config
import qualified Data       as D
import qualified Database   as DB
import qualified Client     as C


type FeedAPI = "feed" :> Get '[JSON] [D.Post]

feedAPI :: Proxy FeedAPI
feedAPI = Proxy

feedServer :: Config -> Server FeedAPI
feedServer config = getFeed
  where getFeed :: Handler [D.Post]
        getFeed = liftIO $ C.feed config
