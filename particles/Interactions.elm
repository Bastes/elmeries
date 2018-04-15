module Mail exposing (..)

import Html exposing (Html)
import Svg exposing (svg, circle, node, line, radialGradient, stop, defs)
import Svg.Attributes exposing (viewBox, width, height, cx, cy, x1, x2, y1, y2, r, stroke, fill, style, id, offset, stopColor, stopOpacity)
import Task
import Time exposing (Time, second)
import Math.Vector2 exposing (Vec2, vec2, add, sub, getX, getY, normalize, scale, distance, direction)
import Array exposing (get, fromList)
import Maybe exposing (withDefault, andThen)
import Random
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
    { screen : Window.Size
    , particles : List Particle
    }


type alias Particle =
    { position : Vec2
    , speed : Vec2
    }


init : ( Model, Cmd Msg )
init =
    ( { screen = { width = 0, height = 0 }
      , particles = []
      }
    , Task.perform Init Window.size
    )



-- UPDATE


type Msg
    = Init Window.Size
    | ParticleInit (List Particle)
    | Resize Window.Size
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init ({ width, height } as screen) ->
            let
                toParticle ( x, y ) =
                    Particle
                        (vec2 (x |> toFloat) (y |> toFloat))
                        (vec2 0 0)
            in
                ( { model | screen = screen }
                , Random.pair
                    (Random.int 0 width)
                    (Random.int 0 height)
                    |> Random.map toParticle
                    |> Random.list 100
                    |> Random.generate ParticleInit
                )

        ParticleInit particles ->
            ( { model | particles = particles }
            , Cmd.none
            )

        Resize screen ->
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
        , Window.resizes Resize
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
        svg
            [ svgViewBox
            , svgStyle
            ]
            (model.particles
                |> List.map particleView
            )


particleView : Particle -> Html Msg
particleView particle =
    circle
        [ cx (getX particle.position |> toString)
        , cy (getY particle.position |> toString)
        , r "5"
        , fill "black"
        ]
        []
