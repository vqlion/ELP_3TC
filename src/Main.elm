module Main exposing(..)
-- Make a GET request to load a book called "Public Opinion"
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/http.htmls

import Browser
import Http
import Html exposing (..)
import Html.Events exposing (onInput,onClick)
import Html.Attributes exposing (..)
import Http
import Random
import Task
import Json.Decode as Decode exposing  (..)
import Svg
import Browser.Navigation exposing (load)

-- import Debug


------------------------MAIN------------------------


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

------------------------MODEL------------------------

type alias Model =
  { def_list : WordDefinitions
  , word_list : List String
  , selectedWord : String
  , state : State 
  , guessed : String
  , load : String
  , newLoad : String
  }
type State
  = Failure
  | Loading
  | Success (WordDefinitions)

------------------------TYPES------------------------

type alias WordDefinition =
  {
    word : String
  , meanings : List Meaning
  }

type alias Definitions = 
  {
    definition : String 
  }

type alias Meaning = 
  {
    partOfSpeech : String 
  , definitions : List Definitions
  }

type alias WordDefinitions = List WordDefinition 
type alias Meanings = List Meaning
type alias ListDefinitions = List Definitions

type Msg
  = GotWord (Result Http.Error String)
  | Random_nb Int 
  | GotDefinition (Result Http.Error WordDefinitions)
  | Guessed String
  | Change String
  | Next

----------------------DECODERS----------------------

recupereJson : Decode.Decoder WordDefinitions
recupereJson = 
  Decode.list wordDecoder

wordDecoder : Decode.Decoder WordDefinition
wordDecoder =
  Decode.map2 WordDefinition
    (Decode.field "word" Decode.string )
    (Decode.field "meanings" <| Decode.list meaningsDecoder)

meaningsDecoder : Decode.Decoder Meaning
meaningsDecoder = 
  Decode.map2 Meaning 
    (Decode.field "partOfSpeech" Decode.string)
    (Decode.field "definitions" <| Decode.list definitionsDecoder)

definitionsDecoder : Decode.Decoder Definitions
definitionsDecoder = 
  Decode.map Definitions
    (Decode.field "definition" Decode.string) 

---------------GETTING WORD AND DEFINITION---------------

init : () -> (Model, Cmd Msg)
init _ =
  ( Model [] [] " " Loading  " " " " " "
  , Http.get
      { url = "../words.txt"
      , expect = Http.expectString GotWord
      }
  )

-------------------------UPDATE-------------------------
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotWord (Ok text) ->
      ({
        model | word_list = String.split " " text
      }
      ,
        Random.generate Random_nb (Random.int 0 999)
      )

    GotWord(Err _) ->
      ({model| state = Failure}
      , Cmd.none
      )

    Random_nb nb->
      let mot=Maybe.withDefault " " (List.head (List.drop nb model.word_list))
      in ({
        model|selectedWord=mot}, Http.get
          { url = ("https://api.dictionaryapi.dev/api/v2/entries/en/"++mot)
          , expect = Http.expectJson GotDefinition recupereJson
          })
      
    GotDefinition def ->
      case def of
        Ok define -> 
          ({
            model | def_list = define , state = Success (define)
          }
          , Cmd.none)

        Err _ ->
          ({
            model| state = Failure
          }
          , Cmd.none)

    Guessed expr->
      let guess=model.selectedWord
      in ({model|guessed=guess}, Cmd.none)
    
    Change message-> 
      let load=""
      in ({model|load=message, newLoad ="You guessed the word ! Congratulations ! You can restart now "}, Cmd.none)
    
    Next -> (model, Random.generate Random_nb (Random.int 0 999))
     
---------------------SUBSCRIPTIONS---------------------

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-------------------------VIEW--------------------------

view : Model -> Html Msg
view model =
  case model.state of
    Failure ->
      div[][
        text ("I was unable to load your book.")
      ]
    Loading ->
      text "Loading..."

    Success (result) -> 
      div [ style "display" "flex"
          , style "flex-direction" "column"
          , style "align-items" "center"
          , style "justify-content" "space-evenly"
          , style "gap" "1vh"
          , style "background-color" "lightcoral"
          , style "width" "fit-content"
          , style "min-width" "30vw"
          , style "max-width" "75vw"
          , style "margin" "auto"
          , style "margin-top" "5vh"
          , style "margin-bottom" "5vh"
          , style "box-shadow" "4px 4px 2px black"
          , style "padding" "20px"
          , style "font-family" "arial"
          ] [
        h1 [ style "margin" "0" ] [(welcomeMessage)],
        h3 [ style "margin" "0" ] [text "Try to guess the word below based on its definition"],
        div[ style "color" "black", style "font-size" "large",  style "font-family" "auto" ] (convertData result),
        div [ style "text-transform" "capitalize", style "font-size" "large", style "font-family" "auto" ] [text (model.guessed)],
        div [ style "display" "flex"
            , style "flex-direction" "column"
            , style "justify-content" "center"
            ]
          [ 
            input [ placeholder "Guess the word", Html.Attributes.value model.load, onInput Change, style "width" "30%", style "margin" "auto", style "margin-bottom" "10px" ] []
          ,
            if String.toLower model.load==String.toLower model.selectedWord then 
            div [ style "font-size" "large" ] [ text ( model.newLoad) ]
            else 
            div [ style "font-size" "large" ] [  ( failureMessage) ]
          ],
          div [ style "display" "flex", style "gap" "20px" ] [
            div [ style "font-size" "large" ] [convert(button [ onClick (Guessed)] [ text "Show answer" ])],
            div [ style "font-size" "large" ] [button [ onClick (Next)] [ text "Get a new word" ]]
          ]
        ]  

convert : Html (String -> Msg) -> Html Msg
convert html =
    Html.map (\toMsg -> toMsg "") html
    
viewMeaning : Meaning -> Html Msg 
viewMeaning display1 = 
  li [ style "font-weight" "bold" 
     , style "text-transform" "capitalize"
     , style "padding" "5px"
     ]
    [text display1.partOfSpeech
    ,ul [ style "font-weight" "normal" 
        , style "text-transform" "none"
        , style "list-style" "arabic"
        ] 
        (List.map viewDefinition display1.definitions)
    ]

viewDefinition : Definitions -> Html Msg
viewDefinition display2 =
  li [] 
    [div [ style "font-style" "italic" 
         , style "padding-left" "5px" 
         ] [text display2.definition]]

viewWordDefinition : WordDefinition -> Html Msg
viewWordDefinition display3 = 
  div []
  [
    ul [ style "list-style" "none" ] (List.map viewMeaning display3.meanings)
  ]

convertData : WordDefinitions -> List (Html Msg)
convertData data =
   List.map (\display3 -> viewWordDefinition display3) data


-------------------------GAME-------------------------

welcomeMessage : Html Msg
welcomeMessage =
  div[] 
  [
    text "Welcome to guess the word!"
  ]

failureMessage : Html Msg
failureMessage =
  div[] 
  [
    text "You didn't guess the word, try again or skip to the next one"
  ]


