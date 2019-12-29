{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Text
import Data.Time (UTCTime)
import Servant.API

import Data
import NoteServer

import System.Directory (listDirectory, getModificationTime)

import Network.Wai.Handler.Warp

main :: IO ()
main = do
  a <- app
  run 8081 a

