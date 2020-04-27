module Main exposing (main)

import Browser
import Css exposing (auto, border2, displayFlex, flex, height, margin, marginRight, maxWidth, num, padding, pct, px, solid, width)
import Dict exposing (Dict)
import Html.Styled exposing (Html, div, h1, h3, option, p, select, span, text, textarea, toUnstyled)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onInput)
import Keyboard exposing (RawKey)



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view >> toUnstyled, subscriptions = subscriptions }



-- MODEL


type Layout
    = Neo
    | AdnW


type alias Model =
    { text : String
    , convertedText : String
    , layout : Layout
    , position : Int
    }


init : flags -> ( Model, Cmd msg )
init _ =
    ( { text = ""
      , convertedText = ""
      , layout = Neo
      , position = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = TextChanged String
    | LayoutChanged String
    | KeyDown RawKey


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    let
        newModel =
            case msg of
                TextChanged newText ->
                    let
                        newConvertedText =
                            convertText newText model.layout
                                |> String.replace "\n" " "
                    in
                    { model | text = newText, convertedText = newConvertedText, position = 0 }

                LayoutChanged newLayout ->
                    if newLayout == "Neo" then
                        { model | layout = Neo, convertedText = convertText model.text Neo |> String.replace "\n" " ", position = 0 }

                    else if newLayout == "AdnW" then
                        { model | layout = AdnW, convertedText = convertText model.text AdnW |> String.replace "\n" " ", position = 0 }

                    else
                        model

                KeyDown key ->
                    if Keyboard.rawValue key == String.slice model.position (model.position + 1) model.convertedText then
                        if model.position >= String.length model.text - 1 then
                            { model | position = 0 }

                        else
                            { model | position = model.position + 1 }

                    else
                        model
    in
    ( newModel, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ css [ maxWidth (px 800), margin auto, padding (px 10) ] ]
        [ h1 [] [ text "Belegungstester" ]
        , p []
            [ text "Ich tippe mit QWERTZ und würde gerne wissen, wie sich das Tippen mit "
            , select [ onInput LayoutChanged ]
                [ option [] [ text "Neo" ]
                , option [] [ text "AdnW" ]
                ]
            , text " anfühlt."
            ]
        , div
            [ css [ displayFlex ] ]
            [ div [ css [ flex (num 1), marginRight (px 10) ] ]
                [ h3 [] [ text "Eingabe" ]
                , markPositionInText model.position model.convertedText
                ]
            , div [ css [ flex (num 1) ] ]
                [ h3 [] [ text "Ergebnis" ]
                , textarea
                    [ value model.text
                    , onInput TextChanged
                    , css [ width (pct 100), height (px 200) ]
                    ]
                    []
                ]
            ]
        ]


markPositionInText : Int -> String -> Html msg
markPositionInText position givenText =
    div []
        [ text (String.slice 0 position givenText)
        , span [ css [ border2 (px 1) solid ] ] [ text (String.slice position (position + 1) givenText) ]
        , text (String.slice (position + 1) (String.length givenText) givenText)
        ]


convertText : String -> Layout -> String
convertText originalText layout =
    let
        replaceCharacter : Char -> Char
        replaceCharacter input =
            case layout of
                Neo ->
                    Maybe.withDefault input (Dict.get input layoutConversionNeo)

                AdnW ->
                    Maybe.withDefault input (Dict.get input layoutConversionAdnW)
    in
    String.map replaceCharacter originalText



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Keyboard.downs KeyDown ]



-- Layouts


layoutConversionNeo : Dict Char Char
layoutConversionNeo =
    Dict.fromList
        [ ( 'x', 'q' )
        , ( 'v', 'w' )
        , ( 'l', 'e' )
        , ( 'c', 'r' )
        , ( 'w', 't' )
        , ( 'k', 'z' )
        , ( 'h', 'u' )
        , ( 'g', 'i' )
        , ( 'f', 'o' )
        , ( 'q', 'p' )
        , ( 'ß', 'ü' )
        , ( 'u', 'a' )
        , ( 'i', 's' )
        , ( 'a', 'd' )
        , ( 'e', 'f' )
        , ( 'o', 'g' )
        , ( 's', 'h' )
        , ( 'n', 'j' )
        , ( 'r', 'k' )
        , ( 't', 'l' )
        , ( 'd', 'ö' )
        , ( 'y', 'ä' )
        , ( 'ü', 'y' )
        , ( 'ö', 'x' )
        , ( 'ä', 'c' )
        , ( 'p', 'v' )
        , ( 'z', 'b' )
        , ( 'b', 'n' )
        , ( 'j', '-' )
        , ( 'X', 'Q' )
        , ( 'V', 'W' )
        , ( 'L', 'E' )
        , ( 'C', 'R' )
        , ( 'W', 'T' )
        , ( 'K', 'Z' )
        , ( 'H', 'U' )
        , ( 'G', 'I' )
        , ( 'F', 'O' )
        , ( 'Q', 'P' )
        , ( 'U', 'A' )
        , ( 'I', 'S' )
        , ( 'A', 'D' )
        , ( 'E', 'F' )
        , ( 'O', 'G' )
        , ( 'S', 'H' )
        , ( 'N', 'J' )
        , ( 'R', 'K' )
        , ( 'T', 'L' )
        , ( 'D', 'Ö' )
        , ( 'Y', 'Ä' )
        , ( 'Ü', 'Y' )
        , ( 'Ö', 'X' )
        , ( 'Ä', 'C' )
        , ( 'P', 'V' )
        , ( 'Z', 'B' )
        , ( 'B', 'N' )
        , ( 'J', '_' )
        ]


layoutConversionAdnW : Dict Char Char
layoutConversionAdnW =
    Dict.fromList
        [ ( 'k', 'q' )
        , ( 'u', 'w' )
        , ( 'ü', 'e' )
        , ( '.', 'r' )
        , ( 'ä', 't' )
        , ( 'v', 'z' )
        , ( 'g', 'u' )
        , ( 'c', 'i' )
        , ( 'l', 'o' )
        , ( 'j', 'p' )
        , ( 'f', 'ü' )
        , ( 'h', 'a' )
        , ( 'i', 's' )
        , ( 'e', 'd' )
        , ( 'a', 'f' )
        , ( 'o', 'g' )
        , ( 'd', 'h' )
        , ( 't', 'j' )
        , ( 'r', 'k' )
        , ( 'n', 'l' )
        , ( 's', 'ö' )
        , ( 'ß', 'ä' )
        , ( 'x', 'y' )
        , ( 'y', 'x' )
        , ( 'ö', 'c' )
        , ( ',', 'v' )
        , ( 'q', 'b' )
        , ( 'p', 'n' )
        , ( 'p', 'm' )
        , ( 'w', ',' )
        , ( 'm', '.' )
        , ( 'z', '-' )
        , ( 'K', 'Q' )
        , ( 'U', 'W' )
        , ( 'Ü', 'E' )
        , ( 'Ä', 'T' )
        , ( 'V', 'Z' )
        , ( 'G', 'U' )
        , ( 'C', 'I' )
        , ( 'L', 'O' )
        , ( 'J', 'P' )
        , ( 'F', 'Ü' )
        , ( 'H', 'A' )
        , ( 'I', 'S' )
        , ( 'E', 'D' )
        , ( 'A', 'F' )
        , ( 'O', 'G' )
        , ( 'D', 'H' )
        , ( 'T', 'J' )
        , ( 'R', 'K' )
        , ( 'N', 'L' )
        , ( 'S', 'Ö' )
        , ( 'X', 'Y' )
        , ( 'Y', 'X' )
        , ( 'Ö', 'C' )
        , ( 'Q', 'B' )
        , ( 'P', 'N' )
        , ( 'P', 'M' )
        , ( 'W', ';' )
        , ( 'M', ':' )
        , ( 'Z', '_' )
        ]
