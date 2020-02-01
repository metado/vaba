{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (Config(..), loadConfig) where

import qualified Data.Text as T

import Dhall

data Config = Config { notesDir :: FilePath, staticDir :: FilePath }
    deriving (Generic, Show)

instance FromDhall Config

loadConfig :: FilePath -> IO Config
loadConfig path = input auto $ T.pack path
