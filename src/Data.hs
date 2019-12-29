{-# LANGUAGE DeriveGeneric #-}

module Data where

import GHC.Generics

import Data.Time (UTCTime)

import Data.Aeson
import Data.Aeson.Types

import Data.Text
import Servant.API

import System.Directory (listDirectory, getModificationTime)


data Note = Note {
  title :: String,
  content :: String,
  pubDate :: UTCTime
} deriving (Eq, Show, Generic)

instance ToJSON Note

defaultDir = "notes"

listNotes :: IO [Note]
listNotes = do 
  files <- listDirectory defaultDir
  traverse initNote $ fmap (\f -> defaultDir ++ "/" ++ f) files

initNote :: FilePath -> IO Note
initNote path = do
  time <- getModificationTime path
  return $ Note path path time


