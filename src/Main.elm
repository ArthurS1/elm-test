module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

type alias Model = Int

main = Browser.sandbox {init = init, update = update, view = view}

type Msg = Increment | Decrement

init : Model
init = 0

update : Msg -> Model -> Model
update msg model =
  case msg of
      Increment ->
        model + 1
      Decrement ->
        model - 1

view : Model -> Html Msg
view model =
  div []
    [button [onClick Decrement] [text "-"]
    , div [] [text (String.fromInt model)]
    , button [onClick Increment] [text "+"]
    ]
