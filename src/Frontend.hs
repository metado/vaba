{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Frontend where

import           Elm.Derive   (defaultOptions, deriveBoth)
import           Servant.Elm  (DefineElm (DefineElm), Proxy (Proxy), defElmImports, defElmOptions,
                               generateElmModuleWith)

import           Data
import           Api.Server
import           Api.PostApi

generate :: IO ()
generate =
  generateElmModuleWith defElmOptions [ "Generated" , "PostAPI" ]
    defElmImports
    "elm-src"
    [ DefineElm (Proxy :: Proxy Post), DefineElm (Proxy :: Proxy ResultMessage) ]
    (postAPI)
