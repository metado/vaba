module Generated.NoteAPI exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder

type alias Note  =
   { noteId: String
   , title: String
   , content: String
   , pubDate: Posix
   }

jsonDecNote : Json.Decode.Decoder ( Note )
jsonDecNote =
   Json.Decode.succeed (\pnoteId ptitle pcontent ppubDate -> {noteId = pnoteId, title = ptitle, content = pcontent, pubDate = ppubDate})
   |> required "noteId" (Json.Decode.string)
   |> required "title" (Json.Decode.string)
   |> required "content" (Json.Decode.string)
   |> required "pubDate" (jsonDecPosix)

jsonEncNote : Note -> Value
jsonEncNote  val =
   Json.Encode.object
   [ ("noteId", Json.Encode.string val.noteId)
   , ("title", Json.Encode.string val.title)
   , ("content", Json.Encode.string val.content)
   , ("pubDate", jsonEncPosix val.pubDate)
   ]


getNotes : (Result Http.Error  ((List Note))  -> msg) -> Cmd msg
getNotes toMsg =
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
                    [ "notes"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecNote))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getNotesById : String -> (Result Http.Error  ((Maybe Note))  -> msg) -> Cmd msg
getNotesById capture_id toMsg =
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
                    [ "notes"
                    , (capture_id)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.maybe (jsonDecNote))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postNotes : Note -> (Result Http.Error  (HelloMessage)  -> msg) -> Cmd msg
postNotes body toMsg =
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
                    [ "notes"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncNote body)
            , expect =
                Http.expectJson toMsg jsonDecHelloMessage
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
