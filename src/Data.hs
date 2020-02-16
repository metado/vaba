{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Data where

import           Data.Time (UTCTime)
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple
import           Database.SQLite.Simple.Ok
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToRow
import           Database.SQLite.Simple.FromField
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

class WithId a where
  getId :: a -> Int
  getTime :: a -> UTCTime

data TextMessage = TextMessage { 
  messageId :: Int
, messageBody :: String
, messageTime :: UTCTime
} deriving (Show, Eq)

instance FromRow TextMessage where
  fromRow = TextMessage <$> field <*> field <*> field

instance ToRow TextMessage where
  toRow TextMessage{..} = toRow (messageId, messageBody, messageTime)

instance WithId TextMessage where
  getId = messageId
  getTime = messageTime


data FriendshipRequest = FriendshipRequest {
  requestId :: Int
, requester :: String
, addressee :: String
, requestTime :: UTCTime
} deriving (Show, Eq)

instance FromRow FriendshipRequest where
  fromRow = FriendshipRequest <$> field <*> field <*> field <*> field

instance ToRow FriendshipRequest where
  toRow FriendshipRequest{..} = toRow (requestId, requester, addressee, requestTime)

instance WithId FriendshipRequest where
  getId = requestId
  getTime = requestTime


data Message = Friendship FriendshipRequest | Text TextMessage deriving (Show, Eq)

instance WithId Message where
  getId (Friendship r) = getId r
  getId (Text m) = getId m

  getTime (Friendship r) = getTime r
  getTime (Text m) = getTime m

instance Ord Message where
  compare a b = (getTime a) `compare` (getTime b)

type URL = String

data Actor = Actor {
    actorId :: Int          -- ^ Required by ActivityStreams
  , actorType :: String     -- ^ Required by ActivityStreams

  , actorName :: String
  , actorAddress :: String

  -- Required by ActivityPub
  , actorInbox :: URL
  , actorOutbox :: URL
  -- Recommended by ActivityPub
  , actorFollowing :: URL
  , actorFollowers :: URL
  , actorStreams :: URL     -- TODO: [URL]
} deriving (Eq, Show)

instance FromRow Actor where
  fromRow = Actor <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Actor where
  toRow Actor{..} = toRow (actorId, actorType, actorName, actorAddress)

