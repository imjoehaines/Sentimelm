module Main exposing (..)

import Dict
import Afinn
import Html exposing (Html, main_, div, text, h1, p, button, input, label)
import Html.Events exposing (on)
import Html.Attributes exposing (style, property, contenteditable, name, type_, checked)
import Json.Decode exposing (at, string)


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


type Sentiment
    = Negative
    | Neutral
    | Positive


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
        |> List.map String.trim
        |> List.map scoreForWord
        |> List.sum


scoreForWord : String -> Int
scoreForWord word =
    Maybe.withDefault 0 (Dict.get word Afinn.dict)



-- VIEW


view : Model -> Html Msg
view model =
    main_ [ style mainStyle ]
        [ h1 [ style headerStyle ] [ text "Sprint Mailbox" ]
        , div
            [ style inputStyle
            , contenteditable True
            , on "input" (Json.Decode.map Input textContentDecoder)
            ]
            []
        , div [ style radioGroupStyle ]
            [ label [ style labelStyle ]
                [ input
                    [ style radioStyle
                    , type_ "radio"
                    , name "sentiment"
                    , checked (shouldBeChecked Negative model.score)
                    ]
                    []
                , text "Negative"
                ]
            , label [ style labelStyle ]
                [ input
                    [ style radioStyle
                    , type_ "radio"
                    , name "sentiment"
                    , checked (shouldBeChecked Neutral model.score)
                    ]
                    []
                , text "Neutral"
                ]
            , label [ style labelStyle ]
                [ input
                    [ style radioStyle
                    , type_ "radio"
                    , name "sentiment"
                    , checked (shouldBeChecked Positive model.score)
                    ]
                    []
                , text "Positive"
                ]
            ]
        , button [ style buttonStyle ] [ text "Save" ]
        ]


shouldBeChecked : Sentiment -> Int -> Bool
shouldBeChecked sentiment score =
    case sentiment of
        Negative ->
            if isNegative score then
                True
            else
                False

        Neutral ->
            if not (isNegative score) && not (isPositive score) then
                True
            else
                False

        Positive ->
            if isPositive score then
                True
            else
                False


isNegative : Int -> Bool
isNegative score =
    score < 0


isPositive : Int -> Bool
isPositive score =
    score > 0


textContentDecoder : Json.Decode.Decoder String
textContentDecoder =
    at [ "target", "textContent" ] string


mainStyle : List ( String, String )
mainStyle =
    [ ( "width", "100vw" )
    , ( "height", "100vh" )
    , ( "background-color", "rgb(40, 40, 60)" )
    , ( "display", "flex" )
    , ( "flex-direction", "column" )
    , ( "align-items", "center" )
    , ( "justify-content", "center" )
    , ( "color", "rgb(255, 255, 255)" )
    , ( "font-family", "Helvetica, Arial, sans-serif" )
    ]


headerStyle : List ( String, String )
headerStyle =
    [ ( "text-align", "center" )
    , ( "font-weight", "200" )
    ]


inputStyle : List ( String, String )
inputStyle =
    [ ( "width", "40vw" )
    , ( "margin", "0 auto" )
    , ( "padding", "0.5rem" )
    , ( "background-color", "rgba(255, 255, 255, 0.1)" )
    , ( "border", "thin solid rgba(255, 255, 255, 0.2)" )
    , ( "font-size", "1.5rem" )
    , ( "color", "rgb(255, 255, 255)" )
    , ( "box-sizing", "border-box" )
    ]


radioGroupStyle : List ( String, String )
radioGroupStyle =
    [ ( "width", "40vw" )
    , ( "margin", "1rem auto" )
    , ( "font-size", "1.25rem" )
    , ( "display", "flex" )
    , ( "justify-content", "space-around" )
    ]


labelStyle : List ( String, String )
labelStyle =
    []


radioStyle : List ( String, String )
radioStyle =
    [ ( "vertical-align", "text-top" ) ]


buttonStyle : List ( String, String )
buttonStyle =
    [ ( "width", "40vw" )
    , ( "margin", "0 auto" )
    , ( "padding", "0.5rem" )
    , ( "background-color", "rgba(255, 255, 255, 0.2)" )
    , ( "border", "thin solid rgba(255, 255, 255, 0.1)" )
    , ( "font-size", "1.5rem" )
    , ( "color", "rgb(255, 255, 255)" )
    , ( "cursor", "pointer" )
    ]
