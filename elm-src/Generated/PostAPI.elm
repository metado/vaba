module Generated.PostAPI exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder

type alias Post  =
   { body: String
   , author: String
   , pubDate: Posix
   }

jsonDecPost : Json.Decode.Decoder ( Post )
jsonDecPost =
   Json.Decode.succeed (\pbody pauthor ppubDate -> {body = pbody, author = pauthor, pubDate = ppubDate})
   |> required "body" (Json.Decode.string)
   |> required "author" (Json.Decode.string)
   |> required "pubDate" (jsonDecPosix)

jsonEncPost : Post -> Value
jsonEncPost  val =
   Json.Encode.object
   [ ("body", Json.Encode.string val.body)
   , ("author", Json.Encode.string val.author)
   , ("pubDate", jsonEncPosix val.pubDate)
   ]


getPosts : (Result Http.Error  ((List Post))  -> msg) -> Cmd msg
getPosts toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "posts"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecPost))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postPosts : Post -> (Result Http.Error  (HelloMessage)  -> msg) -> Cmd msg
postPosts body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "posts"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncPost body)
            , expect =
                Http.expectJson toMsg jsonDecHelloMessage
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
