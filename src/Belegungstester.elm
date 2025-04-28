module Belegungstester exposing (main)

import Browser
import Css exposing (auto, border2, color, displayFlex, flex, fontSize, height, margin, marginRight, maxWidth, num, padding, pct, px, rgb, small, solid, width)
import Dict exposing (Dict)
import Html.Styled exposing (Html, div, h1, h3, option, p, select, span, text, textarea, toUnstyled)
import Html.Styled.Attributes exposing (css, selected, value)
import Html.Styled.Events exposing (onInput)
import Keyboard exposing (RawKey)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view >> toUnstyled
        , subscriptions = subscriptions
        }



-- MODEL


type Layout
    = AdnW
    | Bone
    | Dvorak
    | KOY
    | Neo
    | QWERTZ
    | CustomLayout String


type alias Model =
    { text : String
    , convertedText : String
    , inputLayout : Layout
    , outputLayout : Layout
    , customInputLayout : String
    , customOutputLayout : String
    , position : Int
    }


init : flags -> ( Model, Cmd msg )
init _ =
    ( { text = defaultText
      , convertedText = convertText defaultText QWERTZ Neo
      , inputLayout = QWERTZ
      , outputLayout = Neo
      , customInputLayout = qwertz
      , customOutputLayout = qwertz
      , position = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = TextChanged String
    | InputLayoutChanged String
    | OutputLayoutChanged String
    | CustomInputLayoutChanged String
    | CustomOutputLayoutChanged String
    | KeyDown RawKey


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    let
        newModel =
            case msg of
                TextChanged newText ->
                    { model
                        | text = newText
                        , convertedText = convertText newText model.inputLayout model.outputLayout
                        , position = 0
                    }

                InputLayoutChanged newLayout ->
                    if newLayout == "AdnW" then
                        { model
                            | inputLayout = AdnW
                            , convertedText = convertText model.text AdnW model.outputLayout
                            , position = 0
                        }

                    else if newLayout == "Bone" then
                        { model
                            | inputLayout = Bone
                            , convertedText = convertText model.text Bone model.outputLayout
                            , position = 0
                        }

                    else if newLayout == "Dvorak" then
                        { model
                            | inputLayout = Dvorak
                            , convertedText = convertText model.text Dvorak model.outputLayout
                            , position = 0
                        }

                    else if newLayout == "KOY" then
                        { model
                            | inputLayout = KOY
                            , convertedText = convertText model.text KOY model.outputLayout
                            , position = 0
                        }

                    else if newLayout == "Neo" then
                        { model
                            | inputLayout = Neo
                            , convertedText = convertText model.text Neo model.outputLayout
                            , position = 0
                        }

                    else if newLayout == "QWERTZ" then
                        { model
                            | inputLayout = QWERTZ
                            , convertedText = convertText model.text QWERTZ model.outputLayout
                            , position = 0
                        }

                    else if newLayout == "Folgendem" then
                        { model
                            | inputLayout = CustomLayout model.customInputLayout
                            , convertedText = convertText model.text (CustomLayout model.customInputLayout) model.outputLayout
                            , position = 0
                        }

                    else
                        model

                OutputLayoutChanged newLayout ->
                    if newLayout == "AdnW" then
                        { model
                            | outputLayout = AdnW
                            , convertedText = convertText model.text model.inputLayout AdnW
                            , position = 0
                        }

                    else if newLayout == "Bone" then
                        { model
                            | outputLayout = Bone
                            , convertedText = convertText model.text model.inputLayout Bone
                            , position = 0
                        }

                    else if newLayout == "Dvorak" then
                        { model
                            | outputLayout = Dvorak
                            , convertedText = convertText model.text model.inputLayout Dvorak
                            , position = 0
                        }

                    else if newLayout == "KOY" then
                        { model
                            | outputLayout = KOY
                            , convertedText = convertText model.text model.inputLayout KOY
                            , position = 0
                        }

                    else if newLayout == "Neo" then
                        { model
                            | outputLayout = Neo
                            , convertedText = convertText model.text model.inputLayout Neo
                            , position = 0
                        }

                    else if newLayout == "QWERTZ" then
                        { model
                            | outputLayout = QWERTZ
                            , convertedText = convertText model.text model.inputLayout QWERTZ
                            , position = 0
                        }

                    else if newLayout == "Folgendem" then
                        { model
                            | outputLayout = CustomLayout model.customOutputLayout
                            , convertedText = convertText model.text model.inputLayout (CustomLayout model.customOutputLayout)
                            , position = 0
                        }

                    else
                        model

                CustomInputLayoutChanged newCustomInputLayout ->
                    { model
                        | inputLayout = CustomLayout newCustomInputLayout
                        , customInputLayout = newCustomInputLayout
                        , convertedText = convertText model.text (CustomLayout newCustomInputLayout) model.outputLayout
                        , position = 0
                    }

                CustomOutputLayoutChanged newCustomOutputLayout ->
                    { model
                        | outputLayout = CustomLayout newCustomOutputLayout
                        , customOutputLayout = newCustomOutputLayout
                        , convertedText = convertText model.text model.inputLayout (CustomLayout newCustomOutputLayout)
                        , position = 0
                    }

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
        [ css [ maxWidth (px 1024), margin auto, padding (px 20) ] ]
        [ h1 [] [ text "Belegungstester" ]
        , p []
            [ text "Ich tippe mit "
            , select [ onInput InputLayoutChanged ]
                [ option [] [ text "AdnW" ]
                , option [] [ text "Bone" ]
                , option [] [ text "Dvorak" ]
                , option [] [ text "KOY" ]
                , option [] [ text "Neo" ]
                , option [ selected True ] [ text "QWERTZ" ]
                , option [] [ text "Folgendem" ]
                ]
            , text " und würde gerne wissen, wie sich das Tippen mit "
            , select [ onInput OutputLayoutChanged ]
                [ option [] [ text "AdnW" ]
                , option [] [ text "Bone" ]
                , option [] [ text "Dvorak" ]
                , option [] [ text "KOY" ]
                , option [ selected True ] [ text "Neo" ]
                , option [] [ text "QWERTZ" ]
                , option [] [ text "Folgendem" ]
                ]
            , text " anfühlt."
            ]
        , div [ css [ displayFlex ] ]
            [ div [ css [ flex (num 1), marginRight (px 40) ] ]
                [ if model.inputLayout == CustomLayout model.customInputLayout then
                    div []
                        [ textarea
                            [ value model.customInputLayout
                            , onInput CustomInputLayoutChanged
                            , css [ width (px 100), height (px 120) ]
                            ]
                            []
                        ]

                  else
                    text ""
                , if String.length (String.replace "\n" "" model.customInputLayout) /= 70 then
                    div [ css [ fontSize small, color (rgb 255 0 0) ] ]
                        [ text "Eine Belegung sollte aus 70 Zeichen bestehen. 2×(12+12+11)" ]

                  else
                    text ""
                ]
            , div [ css [ flex (num 1) ] ]
                [ if model.outputLayout == CustomLayout model.customOutputLayout then
                    textarea
                        [ value model.customOutputLayout
                        , onInput CustomOutputLayoutChanged
                        , css [ width (px 100), height (px 120) ]
                        ]
                        []

                  else
                    text ""
                , if String.length (String.replace "\n" "" model.customOutputLayout) /= 70 then
                    div [ css [ fontSize small, color (rgb 255 0 0) ] ]
                        [ text "Eine Belegung sollte aus 70 Zeichen bestehen. 2×(12+12+11)" ]

                  else
                    text ""
                ]
            ]
        , div
            [ css [ displayFlex ] ]
            [ div [ css [ flex (num 1), marginRight (px 40) ] ]
                [ h3 [] [ text "Das müsste ich tippen…" ]
                , markPositionInText model.position model.convertedText
                ]
            , div [ css [ flex (num 1) ] ]
                [ h3 [] [ text "…um diesen Text zu erzeugen." ]
                , textarea
                    [ value model.text
                    , onInput TextChanged
                    , css [ width (pct 100), height (px 300) ]
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


convertText : String -> Layout -> Layout -> String
convertText originalText inputLayout outputLayout =
    let
        createConversionDict : String -> String -> Dict Char Char
        createConversionDict inputString outputString =
            let
                cleanInputStringAsList =
                    String.replace "\n" "" inputString
                        |> String.toList

                cleanOutputStringAsList =
                    String.replace "\n" "" outputString
                        |> String.toList

                zip list1 list2 =
                    List.map2 Tuple.pair list1 list2
            in
            zip cleanOutputStringAsList cleanInputStringAsList
                |> Dict.fromList

        replaceCharacter : Char -> Char
        replaceCharacter input =
            Maybe.withDefault input <|
                Dict.get input <|
                    case inputLayout of
                        AdnW ->
                            case outputLayout of
                                AdnW ->
                                    Dict.empty

                                Bone ->
                                    createConversionDict adnw bone

                                Dvorak ->
                                    createConversionDict adnw dvorak

                                KOY ->
                                    createConversionDict adnw koy

                                Neo ->
                                    createConversionDict adnw neo

                                QWERTZ ->
                                    createConversionDict adnw qwertz

                                CustomLayout customOutputLayout ->
                                    createConversionDict adnw customOutputLayout

                        Bone ->
                            case outputLayout of
                                AdnW ->
                                    createConversionDict bone adnw

                                Bone ->
                                    Dict.empty

                                Dvorak ->
                                    createConversionDict bone dvorak

                                KOY ->
                                    createConversionDict bone koy

                                Neo ->
                                    createConversionDict bone neo

                                QWERTZ ->
                                    createConversionDict bone qwertz

                                CustomLayout customOutputLayout ->
                                    createConversionDict bone customOutputLayout

                        Dvorak ->
                            case outputLayout of
                                AdnW ->
                                    createConversionDict dvorak adnw

                                Bone ->
                                    createConversionDict dvorak bone

                                Dvorak ->
                                    Dict.empty

                                KOY ->
                                    createConversionDict dvorak koy

                                Neo ->
                                    createConversionDict dvorak neo

                                QWERTZ ->
                                    createConversionDict dvorak qwertz

                                CustomLayout customOutputLayout ->
                                    createConversionDict dvorak customOutputLayout

                        KOY ->
                            case outputLayout of
                                AdnW ->
                                    createConversionDict koy adnw

                                Bone ->
                                    createConversionDict koy bone

                                Dvorak ->
                                    createConversionDict koy dvorak

                                KOY ->
                                    Dict.empty

                                Neo ->
                                    createConversionDict koy neo

                                QWERTZ ->
                                    createConversionDict koy qwertz

                                CustomLayout customOutputLayout ->
                                    createConversionDict koy customOutputLayout

                        Neo ->
                            case outputLayout of
                                AdnW ->
                                    createConversionDict neo adnw

                                Bone ->
                                    createConversionDict neo bone

                                Dvorak ->
                                    createConversionDict neo dvorak

                                KOY ->
                                    createConversionDict neo koy

                                Neo ->
                                    Dict.empty

                                QWERTZ ->
                                    createConversionDict neo qwertz

                                CustomLayout customOutputLayout ->
                                    createConversionDict neo customOutputLayout

                        QWERTZ ->
                            case outputLayout of
                                AdnW ->
                                    createConversionDict qwertz adnw

                                Bone ->
                                    createConversionDict qwertz bone

                                Dvorak ->
                                    createConversionDict qwertz dvorak

                                KOY ->
                                    createConversionDict qwertz koy

                                Neo ->
                                    createConversionDict qwertz neo

                                QWERTZ ->
                                    Dict.empty

                                CustomLayout customOutputLayout ->
                                    createConversionDict qwertz customOutputLayout

                        CustomLayout customInputLayout ->
                            case outputLayout of
                                AdnW ->
                                    createConversionDict customInputLayout adnw

                                Bone ->
                                    createConversionDict customInputLayout bone

                                Dvorak ->
                                    createConversionDict customInputLayout dvorak

                                KOY ->
                                    createConversionDict customInputLayout koy

                                Neo ->
                                    createConversionDict customInputLayout neo

                                QWERTZ ->
                                    createConversionDict customInputLayout qwertz

                                CustomLayout customOutputLayout ->
                                    createConversionDict customInputLayout customOutputLayout
    in
    String.replace "\n" " " originalText
        |> String.map replaceCharacter



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Keyboard.downs KeyDown ]



-- Layouts


adnw : String
adnw =
    "kuü.ävgcljf´hieaodtrnsß③④xyö,qbpwmzKUÜ•ÄVGCLJF~HIEAODTRNSẞ⑤④XYÖ–QBPWMZ"


bone : String
bone =
    "jduaxphlmwß´ctieobnrsgq③④fvüäöyz,.kJDUAXPHLMWẞ~CTIEOBNRSGQ⑤④FVÜÄÖYZ–•K"


dvorak : String
dvorak =
    "ü,.pyfgctz?/aoeiuhdrnsl-äöqjkxbmwv#Ü;:PYFGCTZß\\AOEIUHDRNSL_ÄÖQJKXBMWV'"


koy : String
koy =
    "k.o,yvgclßz´haeiudtrnsf③④xqäüöbpwmjK•O–YVGCLẞZ~HAEIUDTRNSF⑤④XQÄÜÖBPWMJ"


neo : String
neo =
    "xvlcwkhgfqß´uiaeosnrtdy③④üöäpzbm,.jXVLCWKHGFQẞ~UIAEOSNRTDY⑤④ÜÖÄPZBM–•J"


qwertz : String
qwertz =
    "qwertzuiopü+\nasdfghjklöä#\n<yxcvbnm,.-\n\nQWERTZUIOPÜ*\nASDFGHJKLÖÄ'\n>YXCVBNM;:_"


defaultText : String
defaultText =
    """Der Mann, der in dem Haus wohnt, und die Frau, die mit ihm dort lebt, haben zusammen ein Kind. Das Kind ist sehr neugierig und will immer alles wissen. In dem Buch, das auf dem Tisch liegt, sind viele schöne Geschichten zu finden. Von diesen Geschichten wird eine jeden Abend vorgelesen. Die Familie sitzt dann im Wohnzimmer und hört zu. Bei manchen Stellen lachen sie, bei anderen sind sie ganz still. Nach dem Lesen sprechen sie über das, was sie gehört haben. Was bedeutet die Geschichte? Wie kann man sie verstehen? So verbringen sie ihre Abende und sind glücklich mit dem, was sie haben."""
