module Main exposing (..)

import Dict
import Afinn
import Html exposing (Html, main_, div, text, h1, p, button, textarea, label)
import Html.Events exposing (on, onClick, onInput)
import Html.Attributes exposing (placeholder, disabled, class)
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
        , textarea
            [ onInput Input
            , placeholder "…"
            ]
            []
        , div [ class "labels" ]
            [ button [ class (activeClass model Negative), onClick (Override Negative) ] [ text "Negative" ]
            , button [ class (activeClass model Neutral), onClick (Override Neutral) ] [ text "Neutral" ]
            , button [ class (activeClass model Positive), onClick (Override Positive) ] [ text "Positive" ]
            ]
        , button [ disabled (shouldBeDisabled model) ] [ text "Save" ]
        ]


activeClass : Model -> Sentiment -> String
activeClass model sentiment =
    case model.maybeOverride of
        Just override ->
            if override == sentiment then
                "active"
            else
                ""

        Nothing ->
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
