{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Text
import Data.Time (UTCTime)
import System.Environment (getArgs)
import System.Directory (listDirectory, getModificationTime)

import Servant.API

import Data
import NoteServer
import Config (loadConfig, Config)

import Network.Wai.Handler.Warp

parseArgs :: [String] -> String
parseArgs args = case args of
  h : [] -> h
  _ -> error $ "One argument (config path) expected. Got: " ++ show args

main :: IO ()
main = do
    configPath <- fmap parseArgs getArgs
    config <- loadConfig configPath
    (app config) >>= \a -> run 8081 a
