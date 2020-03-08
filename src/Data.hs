{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-} {-# LANGUAGE OverloadedStrings #-}

module Data where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.TH as ATH
import           Data.List (stripPrefix)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple
import           Database.SQLite.Simple.Ok
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToRow
import           Database.SQLite.Simple.FromField
import qualified Elm.Derive                     as ED
import           GHC.Generics
import           Servant.API                    hiding (Post, URI)
import qualified Elm.Derive                     as ED
import           Text.URI (URI)

import           Config (Config(..))
import           Instances

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
  toRow Actor{..} = toRow (actorId, actorType, actorName, actorAddress, actorInbox, actorOutbox, actorFollowing, actorFollowers, actorStreams)



-- | A full account id that can be looked up on the server
data Account = Account { 
  accountName :: T.Text     -- ^ Name, e.g. "chuwy" in "acct:chuwy@vaba.es"
, accountDomain :: T.Text   -- ^ Domain, e.g. "chuwy" in "acct:chuwy@vaba.es"
} deriving (Eq)

instance Show Account where
  show Account { accountName=n, accountDomain=d } = T.unpack ("acct:" <> n <> "@" <> d)
  
instance FromJSON Account where
  parseJSON (String s) = case (readAccount s) of
    Right acc -> pure acc
    Left err -> prependFailure "parsing Account failed, " (typeMismatch "Object" (String s))
  parseJSON invalid = prependFailure "parsing Account failed, " (typeMismatch "String" invalid)

instance ToJSON Account where
  toJSON = strJson . show
    where strJson = toJSON

instance FromHttpApiData Account where
  parseQueryParam s = case (readAccount s) of
    Left e -> Left $ T.pack e
    Right a -> Right a

-- | Parse 'acct:name@domain:port' into Account
readAccount :: T.Text -> Either String Account
readAccount s = case (wordsWhen (== '@') (T.unpack s)) of
  prefix : domain : [] -> case (stripPrefix acct prefix) of
    Just name -> Right $ Account (T.pack name) (T.pack domain)
    Nothing -> Left "Missing acct: prefix"
  _ -> Left "Expected acct:prefix@domain format"
  where acct = "acct:"
        wordsWhen :: (Char -> Bool) -> String -> [String]
        wordsWhen p s = case dropWhile p s of
                             "" -> []
                             s' -> w : wordsWhen p s''
                                   where (w, s'') = break p s'
        
ownAccount :: Config -> Account
ownAccount config = Account (name config) socket
  where socket = (host config) <> ":" <> (T.pack $ show $ port config)

accountUrl :: Account -> T.Text
accountUrl account = "https://" <> accountDomain account <> "/users/" <> accountName account

inboxUrl :: Account -> T.Text
inboxUrl account = "https://" <> accountDomain account <> "/users/" <> accountName account <> "/inbox"

data Rel = ProfilePage T.Text URI | Self T.Text URI deriving (Eq, Show, Generic)

profilePage :: T.Text
profilePage = "http://webfinger.net/rel/profile-page"

instance ToJSON Rel where
  toJSON (ProfilePage t u) = 
    object ["rel" .= profilePage, "type" .= t, "href" .= u]
  toJSON (Self t u) =
    object ["rel" .= self, "type" .= t, "href" .= u]
    where self :: T.Text
          self = "self"

instance FromJSON Rel where
  parseJSON = undefined


data Fingerprint = Fingerprint {
  subject :: Account
, links :: [Rel]
} deriving (Eq, Show, Generic)

ATH.deriveJSON ATH.defaultOptions ''Fingerprint

