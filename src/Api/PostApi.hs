{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.PostApi (PostAPI, postAPI, postServer) where

import           Control.Monad.Except
import           Servant

import qualified Data as D
import qualified Database as DB
import           Config
import           Api.Feed

type PostAPI = "posts" :> Get '[JSON] [D.Post]
          :<|> "posts" :> ReqBody '[JSON] D.Post :> Post '[JSON] D.ResultMessage

postAPI :: Proxy PostAPI
postAPI = Proxy

postServer :: Config -> Server PostAPI
postServer config = getPosts :<|> postPost
  where getPosts :: Handler [D.Post]
        getPosts = liftIO $ DB.getPosts config

        postPost :: D.Post -> Handler D.ResultMessage
        postPost note = do 
          liftIO $ DB.addPost config note
          return $ D.ResultMessage { D.msg = "Thanks!" }
