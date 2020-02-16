-- Make a GET request to load a book called "Public Opinion"
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/http.html
--
module Main exposing (..)


import Browser
import Html exposing (Html, label, text, pre, div, button, li, ul, h1, input, br)
import Html.Attributes exposing (style, type_, disabled)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Decoder, field, string, int)
import Json.Encode exposing (..)
import Http exposing (..)
import Time exposing (..)
import Task exposing (..)
import Iso8601 exposing (..)

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
  = Failure (List Post) String
  | Loading (List Post) String
  | Success (List Post) String

init : () -> (Model, Cmd (Msg))
init _ =
  ( Loading [] ""
  , Http.get
        {
            url = "/feed",
            expect = Http.expectJson GotText postListDecoder
        }
  )

getTime : Cmd Msg
getTime = 
    Task.perform OnTime Time.now 

-- UPDATE

type Msg = GotText (Result Http.Error (List Post))
  | LoadNew
  | UpdateTime
  | PostForm
  | UpdateForm String
  | Posted (Result Http.Error ())
  | OnTime Time.Posix 

type SuperString = Super String

destructModelToList : Model -> List Post
destructModelToList model =
  case model of
    Failure s _ -> s
    Loading s _ -> s
    Success s _ -> s
  
destructModelToMessage : Model -> String
destructModelToMessage model =
  case model of
    Failure _ s -> s
    Loading _ s -> s
    Success _ s -> s

toUtcString : Time.Posix -> String
toUtcString time =
  String.fromInt (toHour utc time)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Posted _ ->
        ( Loading (destructModelToList model) "", 
            Http.get
            {
                url = "/feed",
                expect = Http.expectJson GotText postListDecoder
            }
        )
    LoadNew ->
        (Loading (destructModelToList model) "", 
            Http.get
            {
                url = "/feed",
                expect = Http.expectJson GotText postListDecoder
            }
        )
    PostForm  ->
        (Loading (destructModelToList model) "",
            Http.post
            {
                url = "/posts",
                body = Json.Encode.object
                    [
                      ( "body", Json.Encode.string (destructModelToMessage model))
                      , ( "author", Json.Encode.string "chuwy 2" )
                      , ( "pubDate", Json.Encode.string "dd" )
                    ]
                    |> Http.jsonBody,
                expect = Http.expectWhatever Posted
            }
        )
    OnTime time -> (Loading (destructModelToList model) "",
            Http.post
            {
                url = "/posts",
                body = Json.Encode.object
                    [
                      ( "body", Json.Encode.string (destructModelToMessage model))
                      , ( "author", Json.Encode.string "chuwy 2" )
                      , ( "pubDate", Json.Encode.string (fromTime time))
                    ]
                    |> Http.jsonBody,
                expect = Http.expectWhatever Posted
            }
        )
    UpdateTime -> 
        (Success (destructModelToList model) (destructModelToMessage model), getTime)
    UpdateForm message -> 
        (Success (destructModelToList model) message, Cmd.none)
    GotText result ->
      case result of
        Ok list ->
          (Success list "", Cmd.none)
        Err e ->
          (Failure [] (Debug.toString e), Cmd.none)


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

updateFormAction : String -> Msg
updateFormAction message = UpdateForm message 



pst : Model -> Html Msg
pst model = div
  [ style "max-width" "400px", style "background-color" "#A05555", style "height" "150px", style "margin" "10px", style "padding" "10px" ] 
  [ h1 [] [text "Post Form"],
    label [][text "Message: "],
    input [type_ "text", onInput updateFormAction ] [],
    br [] [],
    br [] [],
    button [type_ "submit", disabled (String.isEmpty (destructModelToMessage model)), onClick UpdateTime ][ text "submit" ]
  ]

view : Model -> Html Msg
view model =
  case model of
    Failure _ e ->
      text e

    Loading _ _->
      text "Loading..."

    Success list _  ->
      div [style "display" "flex"] [
          div [] [pre [] [
              button [ onClick LoadNew ] [ text "Load New!!" ],
             renderList list ]],
          pst model
          ] 