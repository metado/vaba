{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.WebFinger (WebFingerAPI, webFingerAPI, webFinger) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.TH as ATH
import           GHC.Generics hiding (Meta)
import           Control.Monad.Except
import           Servant hiding (Link)
import           Data.String
import           Data.List (stripPrefix)
import qualified Data.Text as T

import           Config

type URL = String

profilePage :: T.Text
profilePage = "http://webfinger.net/rel/profile-page"

-- | One of possible types of 'rel' property
data Rel = ProfilePage String URL | Self String URL deriving (Eq, Show, Generic)

instance ToJSON Rel where
  toJSON (ProfilePage t u) = 
    object ["rel" .= profilePage, "type" .= t, "href" .= u]
  toJSON (Self t u) =
    object ["rel" .= self, "type" .= t, "href" .= u]
    where self :: T.Text
          self = "self"

instance FromJSON Rel where
  parseJSON = undefined

type WebFingerAPI = ".well-known" :> "webfinger" :> QueryParam "resource" Account :> Get '[JSON] Fingerprint

webFingerAPI :: Proxy WebFingerAPI
webFingerAPI = Proxy

webFinger :: Config -> Server WebFingerAPI
webFinger config (Just account) = response
  where getPosts :: Handler Fingerprint
        getPosts = liftIO $ getOwnFinger config <$ (putStrLn $ "requesting " ++ show account)
        response = if matching then getPosts else throwError err404
        matching = subject (getOwnFinger config) == account
webFinger _ Nothing = throwError err404

getOwnFinger :: Config -> Fingerprint
getOwnFinger config = Fingerprint { subject = Account (name config) socket, links = [rel] } 
  where socket = (host config) ++ ":" ++ show (port config)
        rel = Self "application/activity+json" $ "https://" ++ socket ++ "/users/" ++ name config

readAccount :: String -> Either String Account
readAccount s = case wordsWhen (== '@') s of
  prefix : domain : [] -> case (stripPrefix acct prefix) of
    Just name -> Right $ Account name domain
    Nothing -> Left "Missing acct: prefix"
  _ -> Left "Expected acct:prefix@domain format"
  where acct = "acct:"

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                     "" -> []
                     s' -> w : wordsWhen p s''
                           where (w, s'') = break p s'


-- | A full account id that can be looked up on the server
data Account = Account { 
  accountName :: String     -- ^ Name, e.g. "chuwy" in "acct:chuwy@vaba.es"
, accountDomain :: String   -- ^ Domain, e.g. "vaba.es" in "acct:chuwy@vaba.es"
} deriving (Eq)

-- | A cannonical representation of an Account
instance Show Account where
  show Account { accountName=n, accountDomain=d } = "acct:" ++ n ++ "@" ++ d
  
instance FromJSON Account where
  parseJSON (String s) = case (readAccount $ T.unpack s) of
    Right acc -> pure acc
    Left err -> prependFailure "parsing Coord failed, " (typeMismatch "Object" (String s))
  parseJSON invalid = prependFailure "parsing Account failed, " (typeMismatch "String" invalid)

instance ToJSON Account where
  toJSON = strJson . show
    where strJson = toJSON

instance FromHttpApiData Account where
  parseQueryParam s = case (readAccount $ T.unpack s) of
    Left e -> Left $ T.pack e
    Right a -> Right a


data Fingerprint = Fingerprint {
  subject :: Account
, links :: [Rel]
} deriving (Eq, Show, Generic)

ATH.deriveJSON ATH.defaultOptions ''Fingerprint

