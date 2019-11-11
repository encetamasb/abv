module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, p, input)
import Html.Events exposing (onInput)
import Html.Attributes exposing (value)
import Maybe
import Either exposing (Either(..))


type alias ABVInput =
  { value : Either String Int
  , error : String
  }

type alias Model =
  { srcABV1 : ABVInput
  , srcABV2 : ABVInput
  , targetABV : ABVInput
  , ratio : Either String (Int, Int)
  }


type Msg
  = ChangeABV1 String
  | ChangeABV2 String
  | ChangeTargetABV String


init : Model
init =
  { srcABV1 = ABVInput (Right 12) ""
  , srcABV2 = ABVInput (Right 40) ""
  , targetABV = ABVInput (Right 16) ""
  , ratio = Right (6, 1)
  }

abvInputFromString s =
  case String.toInt s of
    Just i ->
      ABVInput (Right i) ""
    Nothing ->
      ABVInput (Left s) "Invalid number."



toMaybe : Either String Int -> Maybe Int
toMaybe x =
  case x of
    Left _ ->
      Nothing
    Right v ->
      Just v

gcd : Int -> Int -> Int
gcd a b = if b == 0 then a else gcd b (remainderBy a b)

-- x = (c - b) / (a - b)
calcRatio model =
  let
    a = toMaybe model.srcABV1.value
    b = toMaybe model.srcABV2.value
    c = toMaybe model.targetABV.value
    num = Maybe.map2 (-) c b
    den = Maybe.map2 (-) a b
  in
    case den of
      Just 0 ->
        Left "Source ABV values must be different."
      Nothing ->
        Left "Not possible to caclulate."
      Just d ->
        case num of
          Just n ->
            Right (n, d - n)
          Nothing ->
            Left "Not possible to caclulate."

recalcRatio model =
  { model | ratio = calcRatio model}


update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangeABV1 s ->
      recalcRatio { model | srcABV1 = abvInputFromString s }
    ChangeABV2 s ->
      recalcRatio { model | srcABV2 = abvInputFromString s }
    ChangeTargetABV s ->
      recalcRatio { model | targetABV = abvInputFromString s }



inputView f v err =
  div []
    [ input [ value v, onInput f ] []
    , p [] [text err]
    ]


abvInputView f input =
  case input.value of
    Left s ->
      inputView f s input.error
    Right v ->
      inputView f (String.fromInt v) ""


ratioView model =
  case model.ratio of
    Right (m, n) ->
      div []
        [ p [] [text (String.fromInt m ++ ":" ++ String.fromInt n) ] ]
    Left err ->
      div []
        [ p [] [text err] ]

view : Model -> Html Msg
view model =
  div []
    [ abvInputView ChangeABV1 model.srcABV1
    , abvInputView ChangeABV2 model.srcABV2
    , abvInputView ChangeTargetABV model.targetABV
    , ratioView model
    ]


main =
  Browser.sandbox { init = init, update = update, view = view }


