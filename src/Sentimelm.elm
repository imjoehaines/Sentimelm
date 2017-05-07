module Main exposing (..)

import Dict
import Afinn
import Html exposing (Html, input, div, text)
import Html.Events exposing (onInput)
import Html.Attributes exposing (value)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { input : String
    , score : Int
    }


model : Model
model =
    { input = ""
    , score = 0
    }



-- UPDATE


type Msg
    = Input String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input text ->
            { model
                | score = scoreForWords text
                , input = text
            }


scoreForWords : String -> Int
scoreForWords text =
    String.split " " text
        |> List.map scoreForWord
        |> List.sum


scoreForWord : String -> Int
scoreForWord word =
    Maybe.withDefault 0 (Dict.get word Afinn.dict)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput Input, value model.input ] []
        , text (scoreMessage model)
        ]


scoreMessage : Model -> String
scoreMessage model =
    if model.score > 0 then
        "Positive"
    else if model.score < 0 then
        "Negative"
    else if String.length model.input > 0 then
        "Unknown/neutral"
    else
        ""
