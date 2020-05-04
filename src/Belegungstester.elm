module Belegungstester exposing (main)

import Browser
import Css exposing (auto, border2, displayFlex, flex, height, margin, marginRight, maxWidth, num, padding, pct, px, solid, width)
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


type alias Model =
    { text : String
    , convertedText : String
    , inputLayout : Layout
    , outputLayout : Layout
    , position : Int
    }


init : flags -> ( Model, Cmd msg )
init _ =
    ( { text = defaultText
      , convertedText = convertText defaultText QWERTZ Neo
      , inputLayout = QWERTZ
      , outputLayout = Neo
      , position = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = TextChanged String
    | InputLayoutChanged String
    | OutputLayoutChanged String
    | KeyDown RawKey


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    let
        newModel =
            case msg of
                TextChanged newText ->
                    let
                        newConvertedText =
                            convertText newText model.inputLayout model.outputLayout
                    in
                    { model
                        | text = newText
                        , convertedText = newConvertedText
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
            [ text "Ich tippe mit "
            , select [ onInput InputLayoutChanged ]
                [ option [] [ text "AdnW" ]
                , option [] [ text "Bone" ]
                , option [] [ text "Dvorak" ]
                , option [] [ text "KOY" ]
                , option [] [ text "Neo" ]
                , option [ selected True ] [ text "QWERTZ" ]
                ]
            , text " und würde gerne wissen, wie sich das Tippen mit "
            , select [ onInput OutputLayoutChanged ]
                [ option [] [ text "AdnW" ]
                , option [] [ text "Bone" ]
                , option [] [ text "Dvorak" ]
                , option [] [ text "KOY" ]
                , option [ selected True ] [ text "Neo" ]
                , option [] [ text "QWERTZ" ]
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
            Dict.fromList (List.map2 Tuple.pair (String.toList outputString) (String.toList inputString))

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
    in
    String.map replaceCharacter originalText
        |> String.replace "\n" " "



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Keyboard.downs KeyDown ]



-- Layouts


qwertz : String
qwertz =
    "qwertzuiopü+asdfghjklöä#<yxcvbnm,.-QWERTZUIOPÜ*ASDFGHJKLÖÄ'>YXCVBNM;:_"


neo : String
neo =
    "xvlcwkhgfqß´uiaeosnrtdy③④üöäpzbm,.jXVLCWKHGFQẞ~UIAEOSNRTDY⑤④ÜÖÄPZBM–•J"


adnw : String
adnw =
    "kuü.ävgcljf´hieaodtrnsß③④xyö,qbpwmzKUÜ•ÄVGCLJF~HIEAODTRNSẞ⑤④XYÖ–QBPWMZ"


dvorak : String
dvorak =
    "ü,.pyfgctz?/aoeiuhdrnsl-äöqjkxbmwv#Ü;:PYFGCTZß\\AOEIUHDRNSL_ÄÖQJKXBMWV'"


bone : String
bone =
    "jduaxphlmwß´ctieobnrsgq③④fvüäöyz,.kJDUAXPHLMWẞ~CTIEOBNRSGQ⑤④FVÜÄÖYZ–•K"


koy : String
koy =
    "k.o,yvgclßz´haeiudtrnsf③④xqäüöbpwmjK•O–YVGCLẞZ~HAEIUDTRNSF⑤④XQÄÜÖBPWMJ"


defaultText : String
defaultText =
    """der die das in und sein ein zu von haben werden mit an für auf sich nicht es auch er als Jahr neu sein Euro groß haben Uhr erst werden Prozent viel können Deutschland ander müssen Zeit deutsch sagen Tag weit sollen Mensch gut geben Land klein wollen Million eigen kommen Kind letzt gehen Frau hoch machen Mann alt stehen Unternehmen jung lassen Stadt einig sehen Ende zweit finden Berlin vergangen bleiben Woche lange liegen Fall nah zeigen Seite wenig dürfen Mann ander sein Frau klein haben Hand groß werden Tag erst können Auge gut sagen Zeit alt sehen Jahr ganz müssen Kopf lang wollen Gesicht letzt kommen Mutter neu gehen Vater weit machen Kind viel geben Haus einig lassen Blick jung sollen Leben nahe stehen Mensch gerade wissen Tür eigen fragen Wort spät tun Stimme einzig nehmen Herr lieb hören"""
