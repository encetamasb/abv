module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, p, small, span, strong, text)
import Html.Attributes exposing (style, value)
import Html.Entity exposing (rArr)
import Html.Events exposing (onInput)


type alias ABVInput =
    Result ( String, String ) Int


type alias Model =
    { srcABV1 : ABVInput
    , srcABV2 : ABVInput
    , targetABV : ABVInput
    , output : Result String ( Int, Int )
    }


type Msg
    = ABV1Changed String
    | ABV2Changed String
    | TargetABVChanged String


abvInputFromString s =
    case String.toInt s of
        Just i ->
            Ok i

        Nothing ->
            Err ( s, "Invalid integer number." )


calcRatio : Model -> Result String ( Int, Int )
calcRatio model =
    case ( model.srcABV1, model.srcABV2, model.targetABV ) of
        ( Ok a, Ok b, Ok c ) ->
            calcRatio_ a b c

        _ ->
            Err "Not possible to caclulate."


gcd : Int -> Int -> Int
gcd a b =
    if b == 0 then
        a

    else
        gcd b (remainderBy b a)


calcRatio_ : Int -> Int -> Int -> Result String ( Int, Int )
calcRatio_ a b c =
    let
        den =
            a - b

        cond =
            ((a > c) && (c > b)) || ((b > c) && (c > a))
    in
    case ( den, cond ) of
        ( 0, _ ) ->
            Err "Source ABV-s must be different."

        ( _, False ) ->
            Err "Target ABV must be between the range of the two sources."

        ( _, True ) ->
            let
                num =
                    c - b

                gcd_ =
                    gcd den num
            in
            Ok ( num // gcd_, (den - num) // gcd_ )


recalcRatio : Model -> Model
recalcRatio model =
    { model | output = calcRatio model }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ABV1Changed s ->
            recalcRatio { model | srcABV1 = abvInputFromString s }

        ABV2Changed s ->
            recalcRatio { model | srcABV2 = abvInputFromString s }

        TargetABVChanged s ->
            recalcRatio { model | targetABV = abvInputFromString s }


inputView title msg v err =
    div []
        [ span [ style "margin-right" "5px" ] [ text title ]
        , input [ value v, onInput msg ] []
        , text "%"
        , p [] [ text err ]
        ]


abvInputView title msg input =
    case input of
        Err ( v, error ) ->
            inputView title msg v error

        Ok v ->
            inputView title msg (String.fromInt v) ""


toPercent : Int -> Int -> Int
toPercent x y =
    round <| (toFloat x / toFloat y) * 100


outputView model =
    case model.output of
        Ok ( m, n ) ->
            let
                p =
                    toPercent m (m + n)
            in
            div []
                [ text rArr
                , small [ style "margin-left" "5px", style "margin-right" "10px" ]
                    [ strong [] [ text "A " ]
                    , text <| "(~" ++ (String.fromInt <| p) ++ "%)"
                    ]
                , text (String.fromInt m)
                , text ":"
                , text (String.fromInt n)
                , small [ style "margin-left" "10px" ]
                    [ text <| "(~" ++ (String.fromInt <| 100 - p) ++ "%)"
                    , strong [] [ text " B" ]
                    ]
                ]

        Err err ->
            div []
                [ p [] [ text err ] ]


view : Model -> Html Msg
view model =
    div [ style "padding" "10px" ]
        [ abvInputView "A" ABV1Changed model.srcABV1
        , abvInputView "B" ABV2Changed model.srcABV2
        , abvInputView "C" TargetABVChanged model.targetABV
        , outputView model
        ]


init : Model
init =
    { srcABV1 = Ok 12
    , srcABV2 = Ok 40
    , targetABV = Ok 16
    , output = Ok ( 6, 1 )
    }


main =
    Browser.sandbox { init = init, update = update, view = view }
