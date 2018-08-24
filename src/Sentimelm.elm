module Main exposing (Model, Msg(..), Sentiment(..), activeClass, init, initialModel, isNegative, isNeutral, isPositive, main, scoreForWord, scoreForWords, shouldBeDisabled, subscriptions, update, view)

import Afinn
import Browser
import Dict
import Html exposing (Html, button, div, h1, label, main_, p, text, textarea)
import Html.Attributes exposing (class, disabled, placeholder)
import Html.Events exposing (on, onClick, onInput)


main : Program () Model Msg
main =
    Browser.element
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


initialModel : Model
initialModel =
    { score = 0
    , input = ""
    , maybeOverride = Nothing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



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
            ( { score = scoreForWords text
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
        , div [ class "input-container" ]
            [ textarea [ onInput Input, placeholder "…" ] [ text model.input ]
            , p [ class "score-count" ] [ text (String.fromInt model.score) ]
            ]
        , div [ class "labels" ]
            [ button [ class (activeClass model Negative), onClick (Override Negative) ] [ text "Negative" ]
            , button [ class (activeClass model Neutral), onClick (Override Neutral) ] [ text "Neutral" ]
            , button [ class (activeClass model Positive), onClick (Override Positive) ] [ text "Positive" ]
            ]
        , button [ disabled (shouldBeDisabled model.input) ] [ text "Save" ]
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


shouldBeDisabled : String -> Bool
shouldBeDisabled text =
    String.trim text == "" || String.length text < 5
