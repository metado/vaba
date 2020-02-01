{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module Data where

import           Data.Time (UTCTime)
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToRow
import qualified Elm.Derive                     as ED
import           GHC.Generics
import           Servant.API                    hiding (Post)


data Post = Post {
  body :: String,
  author :: String,
  pubDate :: UTCTime
} deriving (Eq, Show, Generic)

instance FromRow Post where
  fromRow = Post <$> field <*> field <*> field

instance ToRow Post where
  toRow Post{..} = toRow (body, author, pubDate)

newtype ResultMessage = ResultMessage { msg :: String } deriving Generic

-- Automatically derive ToJSON and FromJSON
ED.deriveBoth ED.defaultOptions ''Post
ED.deriveBoth ED.defaultOptions ''ResultMessage

data Message = TextMessage UTCTime String | FriendshipRequest UTCTime

