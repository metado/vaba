-- Make a GET request to load a book called "Public Opinion"
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/http.html
--

import Browser
import Html exposing (Html, text, pre, div, button)
import Html.Events exposing (onClick)
import Http

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
  = Failure
  | Loading
  | Success String


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
      {
          url = "https://elm-lang.org/assets/public-opinion.txt",
          expect = Http.expectString GotText
      }
  )

-- UPDATE

type Msg = GotText (Result Http.Error String) | LoadNew

type SuperString = Super String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LoadNew ->
        (Failure, 
            Http.get
            {
                url = "https://elm-lang.org/assets/public-opinion.txt",
                expect = Http.expectString GotText
            }
        )
    GotText result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)
        Err _ ->
          (Failure, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  case model of
    Failure ->
      text "I was unable to load your book."

    Loading ->
      text "Loading..."

    Success fullText ->
      div [] [ 
          button [ onClick LoadNew ] [ text "Load New!!" ],
          pre [] [ text fullText ]] 