{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (Config(..), loadConfig, defaultConfig, ownHost, ownEndpoint) where

import           GHC.Natural (naturalToInt)
import           Control.Monad.Catch
import           Control.Monad.Catch.Pure
import           Control.Lens.Setter (set)
import qualified Data.Text as T
import           Dhall
import qualified Text.URI as U
import qualified Text.URI.Lens as UL

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
ownHost config = unsafeExtract parsed
  where parsed = U.mkURI $ "https://" <> host config <> ":" <> (T.pack $ show $ port config)

-- | ActivityPub endpoin, e.g. "https://localhost:8081/users/bob"
ownEndpoint :: Config -> U.URI
ownEndpoint config = set UL.uriPath (ownPath config) (ownHost config)

-- | Endpoint path piece, e.g. "/users/bob"
ownPath config = [unsafeExtract users, unsafeExtract ownName]
  where users = U.mkPathPiece "users"
        ownName = U.mkPathPiece $ name config

-- | Extract `a` from `MonadThrow a`, safe only for local parsers
unsafeExtract parsed = case (runCatch parsed) of
    Right path -> path
    Left excepiton -> error $ show excepiton

