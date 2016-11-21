module Main exposing (..)

import Html exposing (Html)
import Svg exposing (svg, circle, node, line, radialGradient, stop, defs)
import Svg.Attributes exposing (viewBox, width, height, cx, cy, x1, x2, y1, y2, r, stroke, fill, style, id, offset, stopColor, stopOpacity)
import Window
import Task
import Time exposing (Time, second)
import Math.Vector2 exposing (Vec2, vec2, add, sub, getX, getY, normalize, scale, distance, direction)
import Array exposing (get, fromList)
import Maybe exposing (withDefault, andThen)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Waypoint =
    Vec2


type alias Particle =
    { position : Vec2
    , speed : Vec2
    , waypointIndex : Int
    }


type alias Model =
    { particles : List Particle
    , waypoints : List Waypoint
    , screen : ( Int, Int )
    }


init : ( Model, Cmd Msg )
init =
    ( { particles = [], waypoints = [], screen = ( 0, 0 ) }
    , Task.perform (\dimentions -> WindowInit dimentions) Window.size
    )



-- TOOLING


rotate : Float -> Vec2 -> Vec2
rotate a v =
    let
        ( x, y ) =
            ( getX v, getY v )

        ( c, s ) =
            ( cos a, sin a )
    in
        vec2 (c * x - s * y) (s * x + c * y)


rotateAround : Vec2 -> Float -> Vec2 -> Vec2
rotateAround c a =
    add c << rotate a << sub c



-- UPDATE


type Msg
    = Tick Time
    | WindowInit { width : Int, height : Int }
    | WindowResize { width : Int, height : Int }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ particles, waypoints } as model) =
    case msg of
        Tick _ ->
            let
                newParticles =
                    particles
                        |> updateParticlesWaypointIndex waypoints
                        |> updateParticlesPosition
                        |> attractParticlesTo waypoints
            in
                ( { model | particles = newParticles }, Cmd.none )

        WindowResize { width, height } ->
            ( { model
                | waypoints = initWaypoints { width = width, height = height }
                , screen = ( width, height )
              }
            , Cmd.none
            )

        WindowInit { width, height } ->
            let
                w =
                    toFloat width

                h =
                    toFloat height
            in
                ( { particles =
                        List.foldr (++) [] <|
                            List.map
                                (\y ->
                                    List.map
                                        ((\y x ->
                                            { position = vec2 (w * 0.5 - 200 + 40 * (toFloat x)) (h * 0.5 - 200 + 40 * (toFloat y))
                                            , speed = vec2 0 0
                                            , waypointIndex = 9 * (round (10 * (toFloat (y + x))) % 2)
                                            }
                                         )
                                            y
                                        )
                                        (List.range 0 10)
                                )
                                (List.range 0 10)
                  , waypoints = initWaypoints { width = width, height = height }
                  , screen = ( width, height )
                  }
                , Cmd.none
                )


initWaypoints { width, height } =
    let
        w =
            toFloat width

        h =
            toFloat height
    in
        [ vec2 (w * 0.1) (h * 0.3)
        , vec2 (w * 0.2) (h * 0.7)
        , vec2 (w * 0.3) (h * 0.3)
        , vec2 (w * 0.4) (h * 0.7)
        , vec2 (w * 0.5) (h * 0.3)
        , vec2 (w * 0.6) (h * 0.7)
        , vec2 (w * 0.7) (h * 0.3)
        , vec2 (w * 0.8) (h * 0.7)
        , vec2 (w * 0.9) (h * 0.3)
        , vec2 (w * 0.9) (h * 0.7)
        , vec2 (w * 0.8) (h * 0.3)
        , vec2 (w * 0.7) (h * 0.7)
        , vec2 (w * 0.6) (h * 0.3)
        , vec2 (w * 0.5) (h * 0.7)
        , vec2 (w * 0.4) (h * 0.3)
        , vec2 (w * 0.3) (h * 0.7)
        , vec2 (w * 0.2) (h * 0.3)
        , vec2 (w * 0.1) (h * 0.7)
        ]


updateParticlesWaypointIndex : List Waypoint -> List Particle -> List Particle
updateParticlesWaypointIndex waypoints particles =
    List.map (updateParticleWaypointIndex waypoints) particles


updateParticleWaypointIndex : List Waypoint -> Particle -> Particle
updateParticleWaypointIndex waypoints ({ position, waypointIndex } as particle) =
    let
        waypoint =
            waypointOf waypoints particle

        newWaypointIndex =
            if (distance waypoint position) < 10.0 then
                (waypointIndex + 1) % (List.length waypoints)
            else
                waypointIndex
    in
        { particle | waypointIndex = newWaypointIndex }


attractParticlesTo : List Waypoint -> List Particle -> List Particle
attractParticlesTo waypoints particles =
    List.map (attractParticleTo waypoints) particles


attractParticleTo : List Waypoint -> Particle -> Particle
attractParticleTo waypoints ({ position, speed, waypointIndex } as particle) =
    let
        waypoint =
            waypointOf waypoints particle

        newSpeed =
            position
                |> direction waypoint
                |> scale 0.1
                |> add (scale 0.99 speed)
                |> limitSpeed 5
    in
        { particle | speed = newSpeed }


waypointOf : List Waypoint -> Particle -> Waypoint
waypointOf waypoints { waypointIndex } =
    withDefault (vec2 500 500) <|
        get waypointIndex <|
            fromList waypoints


limitSpeed : Float -> Vec2 -> Vec2
limitSpeed max speed =
    if Math.Vector2.length speed > max then
        speed |> normalize |> scale max
    else
        speed


updateParticlesPosition : List Particle -> List Particle
updateParticlesPosition particles =
    List.map updateParticlePosition particles


updateParticlePosition : Particle -> Particle
updateParticlePosition ({ position, speed } as particle) =
    { particle | position = add position speed }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (second / 60) Tick
        , Window.resizes WindowResize
        ]



-- VIEW


view : Model -> Html Msg
view { particles, waypoints, screen } =
    let
        particleGradient =
            radialGradient [ id "particleGradient" ]
                [ stop [ offset "0%", stopColor "#ffeeff", stopOpacity "1" ] []
                , stop [ offset "25%", stopColor "#770077", stopOpacity "0.5" ] []
                , stop [ offset "50%", stopColor "#770077", stopOpacity "0.25" ] []
                , stop [ offset "100%", stopColor "#770077", stopOpacity "0" ] []
                ]

        d =
            defs [] [ particleGradient ]

        particleViews =
            List.map particleView particles

        ( w, h ) =
            screen

        svgViewBox =
            viewBox <| "0 0 " ++ (toString w) ++ " " ++ (toString h)

        svgStyle =
            style "position: fixed; top: 0; left: 0; width: 100%; height: 100%;"
    in
        svg [ svgViewBox, svgStyle ] (d :: particleViews)


particleView { position, speed } =
    let
        px =
            toString <| getX position

        py =
            toString <| getY position
    in
        circle [ cx px, cy py, r "20", fill "url(#particleGradient)" ] []
