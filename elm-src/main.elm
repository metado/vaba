-- Make a GET request to load a book called "Public Opinion"
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/http.html
--
module Main exposing (..)


import Browser
import Html exposing (Html, label, text, pre, div, button, li, ul, h1, input, br)
import Html.Attributes exposing (style, type_)
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder, field, string, int)
import Json.Encode exposing (..)
import Http exposing (..)

type alias Post  =
   { body: String
   , author: String
   , pubDate: String
   }

postDecoder : Decoder Post
postDecoder =
  Json.Decode.map3 Post
    (field "body" Json.Decode.string)
    (field "author" Json.Decode.string)
    (field "pubDate" Json.Decode.string)


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

type Msg = GotText (Result Http.Error (List Post)) | LoadNew | PostForm | Posted (Result Http.Error ())

type SuperString = Super String


-- expectJson : (Result Error a -> msg) -> Decoder a -> Expect msg
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Posted _ ->
        (Loading, 
            Http.get
            {
                url = "/feed",
                expect = Http.expectJson GotText postListDecoder
            }
        )
    LoadNew ->
        (Loading, 
            Http.get
            {
                url = "/feed",
                expect = Http.expectJson GotText postListDecoder
            }
        )
    PostForm ->
        (Loading,
            Http.post
            {
                url = "/posts",
      {-           body = Http.Body {
                    body = "Hello world 2",
                    author = "chuwy 2",
                    pubDate = "2020-02-01T12:58:36.884891Z"
                }, -}
                body = Json.Encode.object
                    [
                      ( "body", Json.Encode.string "Hello world 2" )
                      , ( "author", Json.Encode.string "chuwy 2" )
                      , ( "pubDate", Json.Encode.string "2020-02-01T12:58:36.884891Z" )
                    ]
                    |> Http.jsonBody,
                expect = Http.expectWhatever Posted
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
   (field "body" Json.Decode.string)

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
renderList lst =  ul [] (List.map(\s -> 
  li [] [ div [] [text s.body, br[][], (text s.author), br[][], (text s.pubDate) ]]) lst)

pst : Html Msg
pst = div
  [ style "max-width" "400px", style "background-color" "#F00" ] 
  [ h1 [] [text "Post Form"],
    label [][text "post here"],
    input [type_ "text"] [],
    button [type_ "submit", onClick LoadNew ][ text "submit" ]
  ]

view : Model -> Html Msg
view model =
  case model of
    Failure e ->
      text e

    Loading ->
      text "Loading..."

    Success list ->
  -- [
      div [] [ 
          button [ onClick LoadNew ] [ text "Load New!!" ],
          button [ onClick PostForm ][ text "submit" ],
          pre [] [ renderList list ],
          pst
          ] 
      
--  ]