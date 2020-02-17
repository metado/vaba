{-# LANGUAGE DeriveGeneric     #-}

module Config (Config(..), loadConfig, defaultConfig) where

import qualified Data.Text as T
import           Dhall

data Config = Config { 
  staticDir :: FilePath
, dbPath :: FilePath 
, name :: String
, host :: String
, port :: Natural
} deriving (Generic, Show)

instance FromDhall Config


loadConfig :: FilePath -> IO Config
loadConfig path = input auto $ T.pack path

defaultConfig :: Config
defaultConfig = Config { 
  staticDir = "./static/"
, dbPath = "test.db"
, name = "bob"
, host = "localhost"
, port = 8081
}
