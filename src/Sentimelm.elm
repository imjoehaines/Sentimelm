module Main exposing (..)

import Dict
import Afinn
import Html exposing (Html, main_, div, text, h1, p, button, input, label)
import Html.Events exposing (on, onInput)
import Html.Attributes exposing (style, property, contenteditable, name, type_, checked, disabled, class, value)
import Json.Decode exposing (at, string)


main : Program Never Model Msg
main =
    Html.program
        { subscriptions = subscriptions
        , view = view
        , update = update
        , init = init
        }



-- MODEL


type alias Model =
    { score : Int
    , input : String
    , maybeOverride : Maybe Sentiment
    }


type Sentiment
    = Negative
    | Neutral
    | Positive


model : Model
model =
    { score = 0
    , input = ""
    , maybeOverride = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = Input String
    | Override Sentiment


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input text ->
            ( { model
                | score = scoreForWords text
                , input = text
                , maybeOverride = Nothing
              }
            , Cmd.none
            )

        Override sentiment ->
            ( { model
                | maybeOverride = Just sentiment
              }
            , Cmd.none
            )


scoreForWords : String -> Int
scoreForWords text =
    String.split " " text
        |> List.map String.trim
        |> List.map scoreForWord
        |> List.sum


scoreForWord : String -> Int
scoreForWord word =
    Dict.get word Afinn.dict
        |> Maybe.withDefault 0



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ h1 [] [ text "Sprint Mailbox" ]
        , div
            [ class "input"
            , contenteditable True
            , on "input" (Json.Decode.map Input textContentDecoder)
            ]
            []
        , div [ class "range-labels" ]
            [ p [ class (activeClass model Negative) ] [ text "Negative" ]
            , p [ class (activeClass model Neutral) ] [ text "Neutral" ]
            , p [ class (activeClass model Positive) ] [ text "Positive" ]
            ]
        , div [ class "range-container" ]
            [ input
                [ type_ "range"
                , class "range"
                , Html.Attributes.min "-10"
                , Html.Attributes.max "10"
                , value (sliderValue model)
                , onInput (sliderChange)
                ]
                []
            ]
        , button [ disabled (shouldBeDisabled model) ] [ text "Save" ]
        ]


sliderValue : Model -> String
sliderValue model =
    case model.maybeOverride of
        Just Negative ->
            "-7"

        Just Positive ->
            "7"

        Just Neutral ->
            "0"

        _ ->
            toString model.score


sliderChange : String -> Msg
sliderChange value =
    let
        intValue =
            Result.withDefault 0 (String.toInt value)
    in
        if isNegative intValue then
            Override Negative
        else if isPositive intValue then
            Override Positive
        else
            Override Neutral


activeClass : Model -> Sentiment -> String
activeClass model sentiment =
    case model.maybeOverride of
        Just Negative ->
            case sentiment of
                Negative ->
                    "active"

                _ ->
                    ""

        Just Positive ->
            case sentiment of
                Positive ->
                    "active"

                _ ->
                    ""

        Just Neutral ->
            case sentiment of
                Neutral ->
                    "active"

                _ ->
                    ""

        _ ->
            case sentiment of
                Negative ->
                    if isNegative model.score then
                        "active"
                    else
                        ""

                Neutral ->
                    if isNeutral model.score then
                        "active"
                    else
                        ""

                Positive ->
                    if isPositive model.score then
                        "active"
                    else
                        ""


isNegative : Int -> Bool
isNegative score =
    score < 0


isNeutral : Int -> Bool
isNeutral score =
    not (isNegative score) && not (isPositive score)


isPositive : Int -> Bool
isPositive score =
    score > 0


textContentDecoder : Json.Decode.Decoder String
textContentDecoder =
    at [ "target", "textContent" ] string


shouldBeDisabled : Model -> Bool
shouldBeDisabled model =
    String.trim model.input == ""
