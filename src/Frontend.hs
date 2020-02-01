{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Frontend where

import           Elm.Derive   (defaultOptions, deriveBoth)

import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Elm  (DefineElm (DefineElm), Proxy (Proxy), defElmImports, defElmOptions,
                               generateElmModuleWith)

import Server
import Data

main :: IO ()
main =
  generateElmModuleWith
    defElmOptions
    [ "Generated" , "PostAPI" ]
    defElmImports
    "elm-src"
    [ DefineElm (Proxy :: Proxy Post) ]
    (noteAPI)
