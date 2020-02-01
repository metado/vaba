{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Frontend where

import           Elm.Derive   (defaultOptions, deriveBoth)

import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Elm  (DefineElm (DefineElm), Proxy (Proxy), defElmImports, defElmOptions,
                               generateElmModuleWith)

import NoteServer
import Data

main :: IO ()
main =
  generateElmModuleWith
    defElmOptions
    [ "Generated" , "NoteAPI" ]
    defElmImports
    "elm-src"
    [ DefineElm (Proxy :: Proxy Note) ]
    (noteAPI)
