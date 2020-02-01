{-# LANGUAGE OverloadedStrings #-}

module Database where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

-- import Config
-- 
-- data TestField = TestField Int String deriving (Show)
-- 
-- instance FromRow TestField where
--   fromRow = TestField <$> field <*> field
-- 
-- printData :: Config -> IO ()
-- printData config = do
--   conn <- open "test.db"
--   execute conn "INSERT INTO test (str) VALUES (?)"
--     (Only ("test string 2" :: String))
--   r <- query_ conn "SELECT * from test" :: IO [TestField]
--   mapM_ print r
--   close conn
