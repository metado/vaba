{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.WebFinger where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Elm.Derive                     as ED
import           GHC.Generics hiding (Meta)
import           Control.Monad.Except
import           Servant hiding (Link)
import           Data.String
import           Data.List (stripPrefix)
import qualified Data.Text as T

import           Config

data Link = Link {
  rel :: String
, linkType :: String
, href :: String
} deriving (Eq, Show, Generic)

ED.deriveBoth ED.defaultOptions ''Link

type WebFingerAPI = ".well-known" :> "webfinger" :> QueryParam "resource" Account :> Get '[JSON] [Fingerprint]

example = Fingerprint { subject = Account "anton" "chuwy.me", links = [] }

webFingerAPI :: Proxy WebFingerAPI
webFingerAPI = Proxy

webFinger :: Config -> Server WebFingerAPI
webFinger config resource = getPosts
  where getPosts :: Handler [Fingerprint]
        getPosts = liftIO $ [example] <$ (putStrLn $ "requesting " ++ show config)

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


data Account = Account { 
  name :: String
, domain :: String
} deriving (Eq)

instance Show Account where
  show Account { name=n, domain=d } = "acct:" ++ n ++ "@" ++ d
  
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
, links :: [Link]
} deriving (Eq, Show, Generic)

ED.deriveBoth ED.defaultOptions ''Fingerprint

