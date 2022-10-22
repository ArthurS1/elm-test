module Main exposing (..)

import Browser
import Http
import Config
import Url.Builder
import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder)
import Json.Decode exposing (Decoder, field, index, string)


-- MAIN

main : Program () Model Msg
main = Browser.element { init = init
                       , update = update
                       , subscriptions = subscriptions
                       , view = view
                       }

-- MODEL

type Weather = Clear
             | FewClouds
             | ScatteredClouds
             | BrokenClouds
             | ShowerRain
             | Rain
             | Thunderstorm
             | Snow
             | Mist

type alias GeoLoc = { lat: Float
                    , long: Float
                    }

type alias Model = { weather: Maybe Weather
                   , description: String
                   , geoloc: Maybe GeoLoc
                   }

type Msg = Reload
         | ChangeLocation String
         | ReceiveLocation (Result Http.Error String)

init : () -> (Model, Cmd Msg)
init _ = (
  { weather = Nothing, description = "no data loaded yet", geoloc = Nothing },
  Cmd.none
  )

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      Reload -> (
        model,
        Http.get {
          url = Url.Builder.crossOrigin
            Config.apiUrl
            ["data", "2.5", "weather"]
            [Url.Builder.string "lat" (String.fromFloat (Maybe.withDefault {lat = 0, long = 0} model.geoloc).lat)
            , Url.Builder.string "lon" (String.fromFloat (Maybe.withDefault {lat = 0, long = 0} model.geoloc).long)
            , Url.Builder.string "appid" Config.apiKey],
          expect = Http.expectJson ReceiveLocation weatherDecoder
        }
        )
      ChangeLocation s -> (
          { model | geoloc = stringToGeoLoc s },
          Cmd.none
        )
      ReceiveLocation result ->
        case result of
            (Ok a) -> ({ model | description = a }, Cmd.none)
            _ -> (model, Cmd.none)

weatherDecoder : Decoder String
weatherDecoder =
  field "weather" (index 0 (field "description" string))

stringToGeoLoc : String -> Maybe GeoLoc
stringToGeoLoc s =
  case String.split "," s of
      [a, b] -> let
                  lat = String.toFloat a
                  long = String.toFloat b
                in
                  case (lat, long) of
                    (Just c, Just d) -> Just {lat = c, long = d}
                    _ -> Nothing
      _ -> Nothing

-- SUBSCRIPTION

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div [] [
      button [onClick Reload] [text "reload data"],
      input [onInput ChangeLocation, placeholder "lat,long"] [],
      text model.description
    ]

