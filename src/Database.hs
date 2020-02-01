{-# LANGUAGE OverloadedStrings #-}

module Database where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import Config
import qualified Data as D

getData :: Config -> IO [D.Post]
getData config = do
  conn <- open $ dbPath config
  r <- query_ conn "SELECT * from posts" :: IO [D.Post]
  mapM_ print r
  close conn
  return r


addData :: Config -> IO ()
addData config = do
  conn <- open $ dbPath config
  execute conn "INSERT INTO posts (str) VALUES (?)"
    (Only ("posts string 2" :: String))
  close conn
