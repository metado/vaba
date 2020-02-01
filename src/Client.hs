{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Client (feed, feedFor) where

import qualified Data.ByteString.Lazy.Internal as Internal 
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as Lazy
import           Data.Aeson
import           Network.HTTP.Simple
import qualified Data.Text as T
import           Data
import           Config

feed :: Config -> IO [Post]
feed config = fmap concat $ mapM feedFor $ friends config

feedFor :: String -> IO [Post]
feedFor ip = do
    responseBody <- loadBodyFor ip
    let decoded = parse responseBody

    case decoded of
        Nothing -> return [] 
        Just posts -> return posts

loadBodyFor :: String -> IO Lazy.ByteString
loadBodyFor ip = do
    response <- makeRequest ip >>= httpLBS

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response

    let responseBody = getResponseBody response

    return responseBody

parse :: Lazy.ByteString -> Maybe [Post]
parse str = decode str

makeRequest :: String -> IO Request
makeRequest ip = parseRequest $ "http://" ++ ip ++ ":8081/posts"

