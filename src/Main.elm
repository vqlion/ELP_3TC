module Main exposing(..)
-- Make a GET request to load a book called "Public Opinion"
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/http.html
--

import Browser
import Html exposing (Html, text, pre, p, div)
import Http
import Html.Attributes exposing (style)
import Html exposing (a)
-- import Debug


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
  | Success (List String)


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
      { url = "../elp_words.txt"
      , expect = Http.expectString GotText
      }
  )



-- UPDATE


type Msg
  = GotText (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          (Success (String.split " " fullText), Cmd.none)

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
      div [] [ text (getElement 657 fullText) ]

getElement : Int -> (List String) -> String
getElement n lst =
  Maybe.withDefault "a" (List.head (List.drop n lst)) 