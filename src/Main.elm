module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, p, small, span, strong, text)
import Html.Attributes exposing (style, value)
import Html.Entity exposing (rArr)
import Html.Events exposing (onInput)
import Maybe


type alias ABVInput =
    { value : Result String Int
    , error : String
    }


type alias Model =
    { srcABV1 : ABVInput
    , srcABV2 : ABVInput
    , targetABV : ABVInput
    , output : Result String ( Int, Int )
    }


type Msg
    = ChangeABV1 String
    | ChangeABV2 String
    | ChangeTargetABV String


abvInputFromString s =
    case String.toInt s of
        Just i ->
            ABVInput (Ok i) ""

        Nothing ->
            ABVInput (Err s) "Invalid integer number."


calcRatio model =
    let
        a =
            Result.toMaybe model.srcABV1.value

        b =
            Result.toMaybe model.srcABV2.value

        c =
            Result.toMaybe model.targetABV.value
    in
    case Maybe.map3 calcRatio_ a b c of
        Nothing ->
            Err "Not possible to caclulate."

        Just r ->
            r


gcd : Int -> Int -> Int
gcd a b =
    if b == 0 then
        a

    else
        gcd b (remainderBy b a)


calcRatio_ a b c =
    let
        num =
            c - b

        den =
            a - b

        gcd_ =
            gcd den num

        cond =
            ((a > c) && (c > b)) || ((b > c) && (c > a))
    in
    case den of
        0 ->
            Err "Source ABV-s must be different."

        _ ->
            case cond of
                False ->
                    Err "Target ABV must be between the range of the two sources."

                True ->
                    Ok ( num // gcd_, (den - num) // gcd_ )


recalcRatio model =
    { model | output = calcRatio model }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeABV1 s ->
            recalcRatio { model | srcABV1 = abvInputFromString s }

        ChangeABV2 s ->
            recalcRatio { model | srcABV2 = abvInputFromString s }

        ChangeTargetABV s ->
            recalcRatio { model | targetABV = abvInputFromString s }


inputView title f v err =
    div []
        [ span [ style "margin-right" "5px" ] [ text title ]
        , input [ value v, onInput f ] []
        , text "%"
        , p [] [ text err ]
        ]


abvInputView title f input =
    case input.value of
        Err s ->
            inputView title f s input.error

        Ok v ->
            inputView title f (String.fromInt v) ""


toPercent : Int -> Int -> Int
toPercent x y =
    round <| (toFloat x / toFloat y) * 100


outputView model =
    case model.output of
        Ok ( m, n ) ->
            div []
                [ text rArr
                , small [ style "margin-left" "5px", style "margin-right" "10px" ]
                    [ strong [] [ text "A " ]
                    , text <| "(~" ++ (String.fromInt <| toPercent m (m + n)) ++ "%)"
                    ]
                , text (String.fromInt m)
                , text ":"
                , text (String.fromInt n)
                , small [ style "margin-left" "10px" ]
                    [ text <| "(~" ++ (String.fromInt <| 100 - toPercent m (m + n)) ++ "%)"
                    , strong [] [ text " B" ]
                    ]
                ]

        Err err ->
            div []
                [ p [] [ text err ] ]


view : Model -> Html Msg
view model =
    div [ style "padding" "10px" ]
        [ abvInputView "A" ChangeABV1 model.srcABV1
        , abvInputView "B" ChangeABV2 model.srcABV2
        , abvInputView "C" ChangeTargetABV model.targetABV
        , outputView model
        ]


init : Model
init =
    { srcABV1 = ABVInput (Ok 12) ""
    , srcABV2 = ABVInput (Ok 40) ""
    , targetABV = ABVInput (Ok 16) ""
    , output = Ok ( 6, 1 )
    }


main =
    Browser.sandbox { init = init, update = update, view = view }
