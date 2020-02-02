{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Applicative
import           Data.List (sort)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

import           Config
import qualified Data as D

getPosts :: Config -> IO [D.Post]
getPosts config = do
  conn <- open $ dbPath config
  r <- query_ conn "SELECT * from posts" :: IO [D.Post]
  close conn
  return r

addPost :: Config -> D.Post -> IO ()
addPost config post = do
  conn <- open $ dbPath config
  execute conn "INSERT INTO posts (body, pubDate, author) VALUES (?, ?, ?)" post
  close conn

readInbox :: Config -> IO [D.Message]
readInbox config = do
  conn <- open $ dbPath config
  r <- query_ conn "SELECT id, requester, addressee, requestTime FROM friendship_requests" :: IO [D.FriendshipRequest]
  m <- query_ conn "SELECT id, body, messageTime FROM text_messages" :: IO [D.TextMessage]
  close conn
  return $ sort $ fmap D.Friendship r ++ fmap D.Text m

addTextMessage :: Config -> D.TextMessage -> IO ()
addTextMessage config message = do
  conn <- open $ dbPath config
  execute conn "INSERT INTO text_messages (id, body, messageTime) VALUES (?, ?, ?)" message
  close conn

addActor :: Config -> D.Actor -> IO ()
addActor config actor = do
  conn <- open $ dbPath config
  execute conn "INSERT INTO text_messages (id, name, address) VALUES (?, ?, ?)" actor
  close conn

listActors :: Config -> IO [D.Actor]
listActors config = do
  conn <- open $ dbPath config
  r <- query_ conn "SELECT id, name, address FROM actors" :: IO [D.Actor]
  close conn
  return r
