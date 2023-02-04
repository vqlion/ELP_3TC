module Main exposing (..)
-- Make a GET request to load a book called "Public Opinion"
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/http.html
--

import Browser
import Array 
import String
import Random
import Html exposing (..)
import Html exposing (Html, Attribute, button, div, form, h1, input, text)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Html.Events exposing (onInput, onClick)
import Http
import Json.Decode exposing (Decoder, decodeString,at, map4, map2, map3, map, field, int, list, string)
-- import Debug


-- MAIN


main =
  Browser.element
    { init = getRandomDef
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL

type State
  = Failure
  | Loading
  | Success 

type alias Model =
  { def_list : List WordDefinition
  , word_list : List String
  , selectedWord : String
  , random_index : Int
  , state : State 
  }

type alias WordDefinition =
  {
    origin : String
  , word : String
  , meanings : List Meaning
  }

type alias Definitions = 
  {
    definition : String 
  , example : String 
  }

type alias Meaning = 
  {
    partOfSpeech : String 
  , definitions : List Definitions
  }

wordDecoder : Decoder WordDefinition
wordDecoder =
  map3 WordDefinition
    (field "origin" string)
    (field "word" string )
    (field "meanings" <| list meaningsDecoder)

meaningsDecoder : Decoder Meaning
meaningsDecoder = 
  map2 Meaning 
    (field "partOfSpeech" string)
    (field "definitions" <| list definitionsDecoder)

definitionsDecoder : Decoder Definitions
definitionsDecoder = 
  map2 Definitions
    (field "definition" string)
    (field "example" string)

type alias WordDefinitions = List WordDefinition 
type alias Meanings = List Meaning
type alias ListDefinitions = List Definitions

type Msg
  = GotDefinition (Result Http.Error WordDefinitions)
  | GotWord (Result Http.Error String)
  | Random_nb Int 
  

-----------------GETTING WORD AND DEFINITION------------------
init : () -> (Model, Cmd Msg)
init _ =
  ( Model [] [] "" 0 Loading
  , Http.get
      { url = "../elp_words.txt"
      , expect = Http.expectString GotWord
      }
  )

getRandomDef : (Model) -> (Model, Cmd Msg)
getRandomDef model =
  ( {model| state=Loading}
  , Http.get
      { url = "https://api.dictionaryapi.dev/api/v2/entries/en/"
    , expect = Http.expectJson GotDefinition (list wordDecoder)
      }
  )

-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotWord (Ok text) ->
      ({
        model | word_list = String.words text
      }
      ,
        Random.generate Random_nb (Random.int 0 999)
      )

    GotWord(Err _) ->
      ({model| state = Failure}
      , Cmd.none
      )

    Random_nb nb->
      let mot=Maybe.withDefault "" (List.head (List.drop nb model.word_list))
      in ({
        model|selectedWord=mot}, Http.get
          { url = "https://api.dictionaryapi.dev/api/v2/entries/en/" ++ mot
          , expect = Http.expectJson GotDefinition (list wordDecoder)
          })
      
    GotDefinition def ->
      case def of
        Ok define -> 
          ({
            model | def_list=define , state = Success 
          }
          , Cmd.none)

        Err _ ->
          ({
            model| state = Failure
          }
          , Cmd.none)

    

 
    -- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


------------------VIEW------------------

view : Model -> Html Msg
view model =
  case model.state of
    Failure ->
      text ("I was unable to load your book.")

    Loading ->
      text "Loading..."

    Success -> 
      div [] [
        text ("hello")
      ]



viewMeaning : Meaning -> Html Msg 
viewMeaning display1 = 
  div []
    [text display1.partOfSpeech
    , ul [] (List.map viewDefinition display1.definitions)
    ]

viewDefinition : Definitions -> Html Msg
viewDefinition display2 =
  div [] 
    [text display2.definition]

viewWordDefinition : WordDefinition -> Html Msg
viewWordDefinition display3 = 
  div []
  [ text display3.origin]

convertData : List WordDefinition -> List (Html Msg)
convertData donnees =
    List.map (\donne -> convertDonne donne) donnees

convertDonne : WordDefinition -> Html Msg
convertDonne donne =
    let
        word = donne.word
        meanings = donne.meanings
    in
        div []
            [ --h2 [] [text word]
             ul [] (List.map viewMeaning meanings)
            ]



-- -- Make a GET request to load a book called "Public Opinion"
-- --
-- -- Read how it works:
-- --   https://guide.elm-lang.org/effects/http.html
-- --

-- import Browser
-- import Html exposing (Html, text, pre, p, div)
-- import Http
-- import Html.Attributes exposing (style)
-- import Html exposing (a)
-- import Html exposing (Html, button, div, text)
-- import Html.Events exposing (onClick)
-- import Json.Decode exposing (Decoder, decodeString,at, map4, map2, map3, map, field, int, list, string)
-- import Random

-- -- import Debug


-- -- MAIN

-- main =
--   Browser.element
--     { init = init
--     , update = update
--     , subscriptions = subscriptions
--     , view = view
--     }

-- ---------------------------WORD---------------------------

-- type alias Model =
--   { word_list : List String
--   , selectedWord : String
--   , state : State
--   }

-- type State
--   = Failure
--   | Loading
--   | Success String


-- type Msg
--   = GotText (Result Http.Error String)
--   | Random_nb Int


-- init : () -> (Model, Cmd Msg)
-- init _ =
--   ( Model [] "" Loading
--   , Http.get
--       { url = "../elp_words.txt"
--       , expect = Http.expectString GotText
--       }
--   )

-- -----------------------JSON-------------------------
-- type alias Model =
--   { word_list : List WordDefinition
--   , selectedWord : String
--   , random_index : Int
--   , state : State 
--   }

-- type alias WordDefinition =
--   {
--     origin : String
--   , word : String
--   , meanings : List Meaning
--   }

-- type alias Definitions = 
--   {
--     definition : String 
--   , example : String 
--   }

-- type alias Meaning = 
--   {
--     partOfSpeech : String 
--   , definitions : List Definitions
--   }

-- wordDecoder : Decoder WordDefinition
-- wordDecoder =
--   map3 WordDefinition
--     (field "origin" string)
--     (field "word" string )
--     (field "meanings" <| list meaningsDecoder)

-- meaningsDecoder : Decoder Meaning
-- meaningsDecoder = 
--   map2 Meaning 
--     (field "partOfSpeech" string)
--     (field "definitions" <| list definitionsDecoder)

-- definitionsDecoder : Decoder Definitions
-- definitionsDecoder = 
--   map2 Definitions
--     (field "definition" string)
--     (field "example" string)

-- type alias WordDefinitions = List WordDefinition 
-- type alias Meanings = List Meaning
-- type alias ListDefinitions = List Definitions

-- type Msg
--   = GotDefinition (Result Http.Error WordDefinition)
--   | Random_nb Int 
  

-- getRandomDef : (Model) -> (Model, Cmd Msg)
-- getRandomDef model =
--   ( {model| state=Loading}
--   , Http.get
--       { url = "https://api.dictionaryapi.dev/api/v2/entries/en/hello"
--     , expect = Http.expectJson GotDefinition wordDecoder
--       }
--   )
-- ----------------------UPDATE------------------------
-- update : Msg -> Model -> (Model, Cmd Msg)
-- update msg model =
--   case msg of
--     GotText (Ok text) ->
--       ({
--         model | word_list = String.words text
--       }
--       ,
--         Random.generate Random_nb (Random.int 0 999)
--       )
--     Random_nb nb->
--       ({model | selectedWord = Maybe.withDefault "" (List.head (List.drop nb model.word_list))
--       }
--       ,
--       Cmd.none)
--     -- FindRandom ->
--     --         (model, Random.generate Random_nb (Random.int 0 (List.length model.word_list - 1)))


--     --     selected = Array.fromList model.word_list
--     --             |> Array.get rn
--     --   in
--     --     ({ model | selected = selected }, Cmd.none)

--     GotText (Err _) ->
--       ({model| state = Failure}
--       , Cmd.none
--       )

-- subscriptions : Model -> Sub Msg
-- subscriptions model =
--   Sub.none



-- -----------------------VIEW--------------------------

-- view : Model -> Html Msg
-- view model =
--   div []
--     [ text "Selected word: "
--     , text model.selectedWord
--     -- ,button [ onClick index ] [ text "Random word" ]
--     ]
