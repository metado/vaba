{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Client (feed, feeed) where

import qualified Data.ByteString.Lazy.Internal as Internal 
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as Lazy
import           Data.Aeson
import           Network.HTTP.Simple
import           Data

feed :: IO [Post]
feed = do
    responseBody <- feeed
    let decoded = parse responseBody

    case decoded of
        Nothing -> undefined
        Just posts -> return posts

feeed = do
    response <- httpLBS "http://httpbin.org/get"

   -- putStrLn $ "The status code was: " ++
   --            show (getResponseStatusCode response)
   -- print $ getResponseHeader "Content-Type" response
   -- L8.putStrLn $ getResponseBody response

    let responseBody = getResponseBody response

    return responseBody

parse :: Lazy.ByteString -> Maybe [Post]
parse str = decode str
