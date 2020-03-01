{-# LANGUAGE DeriveGeneric     #-}

module Config (Config(..), loadConfig, defaultConfig) where

import qualified Data.Text as T
import           Dhall
import qualified Text.URI as U

data Config = Config { 
  staticDir :: FilePath
, dbPath :: FilePath 
, name :: T.Text
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
, name = T.pack "bob"
, host = "localhost"
, port = 8081
}
