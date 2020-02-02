{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Control.Monad
import           System.Environment (getArgs)
import           Network.Wai.Handler.Warp (run)

import           Api.Server (app)
import           Repl (repl)
import qualified Config as Config

data Command = Repl String | Server String

getConfigPath :: Command -> String
getConfigPath (Repl path) = path
getConfigPath (Server path) = path

parseArgs :: [String] -> Command
parseArgs args = case args of
  "server" : config : [] -> Server config
  "repl" : config : [] -> Repl config
  _ -> error $ "One argument (config path) expected. Got: " ++ show args

main :: IO ()
main = do
    command <- fmap parseArgs getArgs
    let configPath = getConfigPath command 
    config <- Config.loadConfig configPath
    case command of
      Server _ -> (app config) >>= \a -> run 8081 a
      Repl _ -> putStrLn "Welcome to the Vaba REPL!" >> forever (repl config)
    
