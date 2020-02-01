{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Data where

import GHC.Generics

import Data.Time (UTCTime)

import Data.Aeson
import Data.Aeson.Types

import CMark (commonmarkToNode, Node(..), NodeType(..))

import Data.Functor.Identity
import qualified Data.Text as T

import qualified Elm.Derive as ED

import Database.SQLite.Simple.FromRow

import Servant.API hiding (Post)

import System.Directory (listDirectory, getModificationTime)

data Post = Post {
  body :: String,
  author :: String,
  pubDate :: UTCTime
} deriving (Eq, Show, Generic)

instance FromRow Post where
  fromRow = Post <$> field <*> field <*> field

newtype HelloMessage = HelloMessage { msg :: String } deriving Generic
instance ToJSON HelloMessage

-- Automatically derive ToJSON and FromJSON
ED.deriveBoth ED.defaultOptions ''Post

getTitle :: Node -> Either String String
getTitle node = case node of
  Node (Just _) DOCUMENT children -> orError "a whole document" children
  Node (Just _) (HEADING 1) children -> orError "a heading" children
  Node (Just _) (TEXT text) _ -> Right $ T.unpack text
  _ -> Left "No title"
  where 
    orError t ns = case ns of
      [] -> Left $ "No nodes in " ++ t
      n : _ -> getTitle n

makeId :: String -> String
makeId = let repl '/' = '-'
             repl '.' = '-'
             repl  c  = c
             strip = dropWhile (== '-')
  in strip . map repl

listPosts :: FilePath -> IO [Post]
listPosts path = undefined

initPost :: FilePath -> IO Post
initPost path = undefined

merge :: Either a a -> a
merge (Right a) = a
merge (Left a) = a
