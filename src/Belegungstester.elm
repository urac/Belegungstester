module Belegungstester exposing (main)

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
    | Dvorak
    | KOY
    | Bone


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
      , layout = AdnW
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
                        { model | layout = Neo, convertedText = convertText model.text Neo, position = 0 }

                    else if newLayout == "AdnW" then
                        { model | layout = AdnW, convertedText = convertText model.text AdnW, position = 0 }

                    else if newLayout == "Dvorak" then
                        { model | layout = Dvorak, convertedText = convertText model.text Dvorak, position = 0 }

                    else if newLayout == "Bone" then
                        { model | layout = Bone, convertedText = convertText model.text Bone, position = 0 }

                    else if newLayout == "KOY" then
                        { model | layout = KOY, convertedText = convertText model.text KOY, position = 0 }

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
                [ option [] [ text "AdnW" ]
                , option [] [ text "Bone" ]
                , option [] [ text "Dvorak" ]
                , option [] [ text "KOY" ]
                , option [] [ text "Neo" ]
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

                Dvorak ->
                    Maybe.withDefault input (Dict.get input layoutConversionDvorak)

                KOY ->
                    Maybe.withDefault input (Dict.get input layoutConversionKOY)

                Bone ->
                    Maybe.withDefault input (Dict.get input layoutConversionBone)
    in
    String.map replaceCharacter originalText
        |> String.replace "\n" " "



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
        , ( 'b', 'm' )
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
        , ( 'B', 'N' )
        , ( 'P', 'M' )
        , ( 'W', ';' )
        , ( 'M', ':' )
        , ( 'Z', '_' )
        ]


layoutConversionDvorak : Dict Char Char
layoutConversionDvorak =
    Dict.fromList
        [ ( 'ü', 'q' )
        , ( ',', 'w' )
        , ( '.', 'e' )
        , ( 'p', 'r' )
        , ( 'y', 't' )
        , ( 'f', 'z' )
        , ( 'g', 'u' )
        , ( 'c', 'i' )
        , ( 't', 'o' )
        , ( 'z', 'p' )
        , ( '?', 'ü' )
        , ( '/', '+' )
        , ( 'o', 's' )
        , ( 'e', 'd' )
        , ( 'i', 'f' )
        , ( 'u', 'g' )
        , ( 'd', 'j' )
        , ( 'r', 'k' )
        , ( 'n', 'l' )
        , ( 's', 'ö' )
        , ( 'l', 'ä' )
        , ( '-', '#' )
        , ( 'ä', '<' )
        , ( 'ö', 'y' )
        , ( 'q', 'x' )
        , ( 'j', 'c' )
        , ( 'k', 'v' )
        , ( 'x', 'b' )
        , ( 'b', 'n' )
        , ( 'w', ',' )
        , ( 'v', '.' )
        , ( '#', '-' )
        , ( 'Ü', 'Q' )
        , ( ';', 'W' )
        , ( ':', 'E' )
        , ( 'P', 'R' )
        , ( 'Y', 'T' )
        , ( 'F', 'Z' )
        , ( 'G', 'U' )
        , ( 'C', 'I' )
        , ( 'T', 'O' )
        , ( 'Z', 'P' )
        , ( 'ß', 'Ü' )
        , ( '\\', '*' )
        , ( 'O', 'S' )
        , ( 'E', 'D' )
        , ( 'I', 'F' )
        , ( 'U', 'G' )
        , ( 'D', 'J' )
        , ( 'R', 'K' )
        , ( 'N', 'L' )
        , ( 'S', 'Ö' )
        , ( 'L', 'Ä' )
        , ( '_', '\'' )
        , ( 'Ä', '>' )
        , ( 'Ö', 'Y' )
        , ( 'Q', 'X' )
        , ( 'J', 'C' )
        , ( 'K', 'V' )
        , ( 'X', 'B' )
        , ( 'B', 'N' )
        , ( 'W', ';' )
        , ( 'V', ':' )
        , ( '\'', '_' )
        ]


layoutConversionBone : Dict Char Char
layoutConversionBone =
    Dict.fromList
        [ ( 'j', 'q' )
        , ( 'd', 'w' )
        , ( 'u', 'e' )
        , ( 'a', 'r' )
        , ( 'x', 't' )
        , ( 'p', 'z' )
        , ( 'h', 'u' )
        , ( 'l', 'i' )
        , ( 'm', 'o' )
        , ( 'w', 'p' )
        , ( 'ß', 'ü' )
        , ( 'c', 'a' )
        , ( 't', 's' )
        , ( 'i', 'd' )
        , ( 'e', 'f' )
        , ( 'o', 'g' )
        , ( 'b', 'h' )
        , ( 'n', 'j' )
        , ( 'r', 'k' )
        , ( 's', 'l' )
        , ( 'g', 'ö' )
        , ( 'q', 'ä' )
        , ( 'f', 'y' )
        , ( 'v', 'x' )
        , ( 'ü', 'c' )
        , ( 'ä', 'v' )
        , ( 'ö', 'b' )
        , ( 'y', 'n' )
        , ( 'z', 'm' )
        , ( 'k', '-' )
        , ( 'J', 'Q' )
        , ( 'D', 'W' )
        , ( 'U', 'E' )
        , ( 'A', 'R' )
        , ( 'X', 'T' )
        , ( 'P', 'Z' )
        , ( 'H', 'U' )
        , ( 'L', 'I' )
        , ( 'M', 'O' )
        , ( 'W', 'P' )
        , ( 'ẞ', 'Ü' )
        , ( 'C', 'A' )
        , ( 'T', 'S' )
        , ( 'I', 'D' )
        , ( 'E', 'F' )
        , ( 'O', 'G' )
        , ( 'B', 'H' )
        , ( 'N', 'J' )
        , ( 'R', 'K' )
        , ( 'S', 'L' )
        , ( 'G', 'Ö' )
        , ( 'Q', 'Ä' )
        , ( 'F', 'Y' )
        , ( 'V', 'X' )
        , ( 'Ü', 'C' )
        , ( 'Ä', 'V' )
        , ( 'Ö', 'B' )
        , ( 'Y', 'N' )
        , ( 'Z', 'M' )
        , ( 'K', '_' )
        ]


layoutConversionKOY : Dict Char Char
layoutConversionKOY =
    Dict.fromList
        [ ( 'k', 'q' )
        , ( '.', 'w' )
        , ( 'o', 'e' )
        , ( ',', 'r' )
        , ( 'y', 't' )
        , ( 'v', 'z' )
        , ( 'g', 'u' )
        , ( 'c', 'i' )
        , ( 'l', 'o' )
        , ( 'ß', 'p' )
        , ( 'z', 'ü' )
        , ( 'h', 'a' )
        , ( 'a', 's' )
        , ( 'e', 'd' )
        , ( 'i', 'f' )
        , ( 'u', 'g' )
        , ( 'd', 'h' )
        , ( 't', 'j' )
        , ( 'r', 'k' )
        , ( 'n', 'l' )
        , ( 's', 'ö' )
        , ( 'f', 'ä' )
        , ( 'x', 'y' )
        , ( 'q', 'x' )
        , ( 'ä', 'c' )
        , ( 'ü', 'v' )
        , ( 'ö', 'b' )
        , ( 'b', 'n' )
        , ( 'p', 'm' )
        , ( 'w', ',' )
        , ( 'm', '.' )
        , ( 'j', '-' )
        , ( 'K', 'Q' )
        , ( 'O', 'E' )
        , ( 'Y', 'T' )
        , ( 'V', 'Z' )
        , ( 'G', 'U' )
        , ( 'C', 'I' )
        , ( 'L', 'O' )
        , ( 'ẞ', 'P' )
        , ( 'Z', 'Ü' )
        , ( 'H', 'A' )
        , ( 'A', 'S' )
        , ( 'E', 'D' )
        , ( 'I', 'F' )
        , ( 'U', 'G' )
        , ( 'D', 'H' )
        , ( 'T', 'J' )
        , ( 'R', 'K' )
        , ( 'N', 'L' )
        , ( 'S', 'Ö' )
        , ( 'F', 'Ä' )
        , ( 'X', 'Y' )
        , ( 'Q', 'X' )
        , ( 'Ä', 'C' )
        , ( 'Ü', 'V' )
        , ( 'Ö', 'B' )
        , ( 'B', 'N' )
        , ( 'P', 'M' )
        , ( 'W', ';' )
        , ( 'M', ':' )
        , ( 'J', '_' )
        ]
