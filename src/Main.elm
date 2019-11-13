module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, p, input, small, span, strong)
import Html.Events exposing (onInput)
import Html.Attributes exposing (value, style)
import Maybe
import Either exposing (Either(..))
import Debug
import Html.Entity exposing (rArr)


type alias ABVInput =
  { value : Either String Int
  , error : String
  }

type alias Model =
  { srcABV1 : ABVInput
  , srcABV2 : ABVInput
  , targetABV : ABVInput
  , output : Either String (Int, Int)
  }


type Msg
  = ChangeABV1 String
  | ChangeABV2 String
  | ChangeTargetABV String


abvInputFromString s =
  case String.toInt s of
    Just i ->
      ABVInput (Right i) ""
    Nothing ->
      ABVInput (Left s) "Invalid integer number."


calcRatio model =
  let
    a = Either.toMaybe model.srcABV1.value
    b = Either.toMaybe model.srcABV2.value
    c = Either.toMaybe model.targetABV.value
  in
    case (Maybe.map3 calcRatio_ a b c) of
      Nothing ->
        Left "Not possible to caclulate."
      Just r ->
        r


gcd : Int -> Int -> Int
gcd a b = if b == 0 then a else gcd b (remainderBy b a)


-- x = (c - b) / (a - b)
calcRatio_ a b c =
  let
    num = c - b
    den = a - b
    gcd_ = gcd den num
    cond = ((a > c) && (c > b)) || ((b > c) && (c > a))
  in
    case den of
      0 ->
        Left "Source ABV-s must be different."
      _ ->
        case cond of
          False ->
            Left "Target ABV must be between the range of the two sources."
          True ->
            Right (num // gcd_, (den - num)  // gcd_)


recalcRatio model =
  { model | output = calcRatio model}


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
    [ span [style "margin-right" "5px"] [text title]
    , input [ value v, onInput f ] []
    , text "%"
    , p [] [text err]
    ]


abvInputView title f input =
  case input.value of
    Left s ->
      inputView title f s input.error
    Right v ->
      inputView title f (String.fromInt v) ""


toPercent : Int -> Int -> Int
toPercent x y = round <| (toFloat x / toFloat y) * 100

outputView model =
  case model.output of
    Right (m, n) ->
      div []
        [ text rArr
        , small [style "margin-left" "5px", style "margin-right" "10px"] [ strong [] [text "A "], text <| "(~" ++ (String.fromInt <| toPercent m (m + n) ) ++ "%)" ]
        , text (String.fromInt m)
        , text ":"
        , text (String.fromInt n)
        , small [style "margin-left" "10px"] [ text <| "(~" ++  (String.fromInt <| 100 - toPercent m (m + n) ) ++ "%)", strong [] [text " B"] ]
        ]
    Left err ->
      div []
        [ p [] [text err] ]


view : Model -> Html Msg
view model =
  div [style "padding" "10px"]
    [ abvInputView "A" ChangeABV1 model.srcABV1
    , abvInputView "B" ChangeABV2 model.srcABV2
    , abvInputView "C" ChangeTargetABV model.targetABV
    , outputView model
    ]


init : Model
init =
  { srcABV1 = ABVInput (Right 12) ""
  , srcABV2 = ABVInput (Right 40) ""
  , targetABV = ABVInput (Right 16) ""
  , output = Right (6, 1)
  }


main =
  Browser.sandbox { init = init, update = update, view = view }
