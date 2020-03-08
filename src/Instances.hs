{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Instances where

import           Data.Aeson
import           Data.Aeson.TH as ATH
import           Dhall
import           Text.URI (URI, renderStr)


instance ToJSON URI where
  toJSON uri = toJSON $ renderStr uri

