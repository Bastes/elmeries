module Mail exposing (..)

import Html exposing (Html)
import Svg exposing (svg, circle, node, line, radialGradient, stop, defs)
import Svg.Attributes exposing (viewBox, width, height, cx, cy, x1, x2, y1, y2, r, stroke, fill, style, id, offset, stopColor, stopOpacity)
import Task
import Time exposing (Time, second)
import Math.Vector2 exposing (Vec2, vec2, add, sub, getX, getY, normalize, scale, distance, direction)
import Array exposing (get, fromList)
import Maybe exposing (withDefault, andThen)
import Window


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { screen : { width : Int, height : Int } }


init : ( Model, Cmd Msg )
init =
    ( { screen = { width = 0, height = 0 } }
    , Task.perform WindowInit Window.size
    )



-- UPDATE


type Msg
    = WindowInit { width : Int, height : Int }
    | WindowResize { width : Int, height : Int }
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowInit ({ width, height } as screen) ->
            ( { model | screen = screen }
            , Cmd.none
            )

        WindowResize ({ width, height } as screen) ->
            ( { model | screen = screen }
            , Cmd.none
            )

        Tick _ ->
            ( model
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (second / 60) Tick
        , Window.resizes WindowResize
        ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        svgViewBox =
            viewBox <| "0 0 " ++ (toString model.screen.width) ++ " " ++ (toString model.screen.height)

        svgStyle =
            style "position: fixed; top: 0; left: 0; width: 100%; height: 100%;"
    in
        svg [ svgViewBox, svgStyle ] []
