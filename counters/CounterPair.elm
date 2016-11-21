module Main exposing (..)

import Counter
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


main =
    Html.beginnerProgram
        { model = init 0 0
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { topCounter : Counter.Model
    , bottomCounter : Counter.Model
    }


init : Int -> Int -> Model
init top bottom =
    { topCounter = Counter.init top
    , bottomCounter = Counter.init bottom
    }



-- UPDATE


type Msg
    = Reset
    | Top Counter.Msg
    | Bottom Counter.Msg


update : Msg -> Model -> Model
update message model =
    case message of
        Reset ->
            init 0 0

        Top msg ->
            { model | topCounter = Counter.update msg model.topCounter }

        Bottom msg ->
            { model | bottomCounter = Counter.update msg model.bottomCounter }



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ Html.map Top (Counter.view model.topCounter)
        , Html.map Bottom (Counter.view model.bottomCounter)
        , button [ onClick Reset ] [ text "RESET" ]
        ]
