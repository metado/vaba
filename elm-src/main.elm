-- Make a GET request to load a book called "Public Opinion"
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/http.html
--
module Main exposing (..)

import Browser
import Html exposing (Html, text, pre, div, button, li, ul, br)
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder, field, string, int)
import Http

type alias Post  =
   { body: String
   , author: String
   , pubDate: String
   }

postDecoder : Decoder Post
postDecoder =
  Json.Decode.map3 Post
    (field "body" string)
    (field "author" string)
    (field "pubDate" string)


postListDecoder : Decoder (List Post)
postListDecoder =
  Json.Decode.list postDecoder

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL

type Model
  = Failure String
  | Loading
  | Success (List Post)


init : () -> (Model, Cmd (Msg))
init _ =
  ( Loading
  , Http.get
        {
            url = "/feed",
            expect = Http.expectJson GotText postListDecoder
        }
  )

-- UPDATE

type Msg = GotText (Result Http.Error (List Post)) | LoadNew

type SuperString = Super String


-- expectJson : (Result Error a -> msg) -> Decoder a -> Expect msg
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LoadNew ->
        (Loading, 
            Http.get
            {
                url = "/feed",
                expect = Http.expectJson GotText postListDecoder
            }
        )
    GotText result ->
      case result of
        Ok list ->
          (Success list, Cmd.none)
        Err e ->
          (Failure (Debug.toString e), Cmd.none)


noteDecoder : Decoder String
noteDecoder =
   (field "body" string)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

headIsu: List Post -> Post
headIsu posts = case (List.head posts) of
  Nothing -> Post "fo" "fo" "dsfgsdfg"
  Just p -> p

renderList : List Post -> Html Msg
renderList lst =  ul [] (List.map(\s -> li [] [ div [] [text s.body, br[][], (text s.author),br[][], (text s.pubDate) ]]) lst)
--renderList lst =  div [] [ text (String.fromInt (List.length lst)) ]

view : Model -> Html Msg
view model =
  case model of
    Failure e ->
      text e

    Loading ->
      text "Loading..."

    Success list ->
      div [] [ 
          button [ onClick LoadNew ] [ text "Load New!!" ],
          pre [] [ renderList list ]] 