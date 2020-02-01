{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           System.Environment (getArgs)
import           Network.Wai.Handler.Warp (run)

import           Api.Server (app)
import qualified Config as Config

parseArgs :: [String] -> String
parseArgs args = case args of
  h : [] -> h
  _ -> error $ "One argument (config path) expected. Got: " ++ show args

main :: IO ()
main = do
    configPath <- fmap parseArgs getArgs
    config <- Config.loadConfig configPath
    (app config) >>= \a -> run 8081 a
