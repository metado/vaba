{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (Config(..), loadConfig, defaultConfig, ownHost, ownPath) where

import           GHC.Natural (naturalToInt)
import qualified Data.Text as T
import           Control.Monad.Catch
import           Control.Monad.Catch.Pure
import           Dhall
import qualified Text.URI as U

data Config = Config { 
  staticDir :: FilePath
, dbPath :: FilePath 
, name :: T.Text
, host :: T.Text
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

ownHost :: Config -> U.URI
ownHost config = case (runCatch parsed) of
    Right uri -> uri
    Left excepiton -> error $ show excepiton
  where parsed = U.mkURI $ "https://" <> host config <> ":" <> (T.pack $ show $ port config)

ownPath config = [unsafeExtract users, unsafeExtract ownName]
  where users = U.mkPathPiece "users"
        ownName = U.mkPathPiece $ name config
        unsafeExtract parsed = case (runCatch parsed) of
          Right path -> path
          Left excepiton -> error $ show excepiton
