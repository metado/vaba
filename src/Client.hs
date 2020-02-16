{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Client (feed, feedFor) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as Lazy
import           Data.Aeson (decode)
import           Data.Maybe (fromMaybe)
import           Network.HTTP.Simple

import           Data
import           Config

feed :: [Actor] -> IO [Post]
feed actors = fmap concat $ mapM feedFor $ map actorAddress actors

feedFor :: String -> IO [Post]
feedFor ip = do
    responseBody <- loadBodyFor ip
    return $ fromMaybe [] $ parse responseBody

loadBodyFor :: String -> IO Lazy.ByteString
loadBodyFor ip = do
    response <- makeRequest ip >>= httpLBS
    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response
    return $ getResponseBody response

parse :: Lazy.ByteString -> Maybe [Post]
parse str = decode str

makeRequest :: String -> IO Request
makeRequest ip = parseRequest $ "http://" ++ ip ++ ":8081/posts"
