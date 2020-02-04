{-# LANGUAGE DeriveGeneric     #-}

module Config (Config(..), loadConfig, defaultConfig) where

import qualified Data.Text as T
import           Dhall

data Config = Config { 
  staticDir :: FilePath
, dbPath :: FilePath 
} deriving (Generic, Show)

instance FromDhall Config


loadConfig :: FilePath -> IO Config
loadConfig path = input auto $ T.pack path

defaultConfig :: Config
defaultConfig = Config { 
  staticDir = "./elm-src/"
, dbPath = "test.db"
}
