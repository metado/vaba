{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Instances where

import           Data.Aeson
import           Data.Aeson.TH as ATH
import           Dhall
import           Text.URI (URI)


instance ToJSON URI where
  toJSON uri = undefined

