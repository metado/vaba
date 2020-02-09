{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Inbox where

import           Control.Monad.Except
import           Servant

import qualified Data as D
import qualified Database as DB
import           Config
import           Api.Feed

type InboxAPI = "inbox" :> Get '[JSON] [D.Post]                                   -- Client-to-server
           :<|> "inbox" :> ReqBody '[JSON] D.Post :> Post '[JSON] D.ResultMessage -- Server-to-server

inboxAPI :: Proxy InboxAPI
inboxAPI = Proxy

inboxEndpoint :: Config -> Server InboxAPI
inboxEndpoint config = getPosts :<|> postPost
  where getPosts :: Handler [D.Post]
        getPosts = liftIO $ DB.getPosts config

        postPost :: D.Post -> Handler D.ResultMessage
        postPost note = do 
          liftIO $ DB.addPost config note
          return $ D.ResultMessage { D.msg = "Thanks!" }
