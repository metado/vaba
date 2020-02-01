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

import Servant.API

import System.Directory (listDirectory, getModificationTime)

data Note = Note {
  noteId :: String,             -- ^ Unique id of a note (filename)
  title :: String,
  content :: String,
  pubDate :: UTCTime
} deriving (Eq, Show, Generic)

newtype HelloMessage = HelloMessage { msg :: String } deriving Generic
instance ToJSON HelloMessage

-- Automatically derive ToJSON and FromJSON
ED.deriveBoth ED.defaultOptions ''Note

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

listNotes :: FilePath -> IO [Note]
listNotes path = do 
  files <- listDirectory path
  traverse initNote $ fmap (\f -> path ++ "/" ++ f) files

initNote :: FilePath -> IO Note
initNote path = do
  time <- getModificationTime path
  content <- readFile path
  let doc = commonmarkToNode [] $ T.pack content
  let title = merge $ getTitle doc
  _ <- print doc
  return $ Note (makeId path) title content time


merge :: Either a a -> a
merge (Right a) = a
merge (Left a) = a
