{-# LANGUAGE OverloadedStrings #-}

module Client (client) where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple
import           Post

feed :: [Post]
feed = do
    response <- httpLBS "http://httpbin.org/get"

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response
