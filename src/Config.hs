{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (Config(..), loadConfig, defaultConfig) where

import qualified Data.Text as T

import Dhall

data Config = Config { 
  notesDir :: FilePath
, staticDir :: FilePath
, dbPath :: FilePath 
} deriving (Generic, Show)

instance FromDhall Config

loadConfig :: FilePath -> IO Config
loadConfig path = input auto $ T.pack path

defaultConfig :: Config
defaultConfig = Config { 
  notesDir = "./notes/"
, staticDir = "./elm-src/"
, dbPath = "test.db"
}
