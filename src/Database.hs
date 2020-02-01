{-# LANGUAGE OverloadedStrings #-}

module Database (getPosts, addPost) where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import Config
import qualified Data as D

getPosts :: Config -> IO [D.Post]
getPosts config = do
  conn <- open $ dbPath config
  r <- query_ conn "SELECT * from posts" :: IO [D.Post]
  mapM_ print r
  close conn
  return r


addPost :: Config -> D.Post -> IO ()
addPost config post = do
  conn <- open $ dbPath config
  execute conn "INSERT INTO posts (body, pubDate, author) VALUES (?, ?, ?)" post
  close conn
