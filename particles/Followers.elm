import Html exposing (Html)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (viewBox, width, height, cx, cy, r, fill)
import Time exposing (Time, second)
import Math.Vector2 exposing (Vec2, vec2, add, getX, getY, normalize, scale, distance, direction)
import Array exposing (get, fromList)
import Maybe exposing (withDefault, andThen)

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Waypoint = Vec2
type alias Particle = { position: Vec2
                      , speed:    Vec2
                      , waypointIndex: Int
                      }
type alias Model    = { particles: List Particle
                      , waypoints: List Waypoint
                      }


init : (Model, Cmd Msg)
init = ( { particles= List.foldr (++) [] <| List.map (\y -> List.map ((\y x -> {position= vec2 (10 * x) (10 * y), speed= vec2 0 0, waypointIndex= 0}) y) [1..10]) [1..10]
         , waypoints= [ vec2 150 150
                      , vec2 350 150
                      , vec2 350 350
                      , vec2 150 350
                      , vec2 250 250
                      ]
         }
       , Cmd.none
       )


-- UPDATE

type Msg = Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ particles, waypoints } as model) =
  case msg of
    Tick _ ->
      let
          newParticles = particles
                      |> updateParticlesWaypointIndex waypoints
                      |> updateParticlesPosition
                      |> updateParticlesSpeed waypoints
      in
        ( { model | particles= newParticles }
        , Cmd.none
        )

updateParticlesWaypointIndex : List Waypoint -> List Particle -> List Particle
updateParticlesWaypointIndex waypoints particles = List.map (updateParticleWaypointIndex waypoints) particles

updateParticleWaypointIndex : List Waypoint -> Particle -> Particle
updateParticleWaypointIndex waypoints ({position, waypointIndex} as particle) =
  let
    maybeWaypoint      = get waypointIndex <| fromList waypoints
    maybeWaypointIndex = maybeWaypoint `andThen` \waypoint ->
      case (distance waypoint position) < 10.0 of
        True  -> Just <| (waypointIndex + 1) % (List.length waypoints)
        False -> Just waypointIndex
    newWaypointIndex = withDefault 0 maybeWaypointIndex
  in
    { particle | waypointIndex= newWaypointIndex }

updateParticlesSpeed : List Waypoint -> List Particle -> List Particle
updateParticlesSpeed waypoints particles = List.map (updateParticleSpeed waypoints) particles

updateParticleSpeed : List Waypoint -> Particle -> Particle
updateParticleSpeed waypoints ({position, speed, waypointIndex} as particle) =
  let
    maybeWaypoint = get waypointIndex <| fromList waypoints
    maybeSpeed    = maybeWaypoint `andThen` \waypoint -> Just
                                                      <| limitSpeed 1
                                                      <| add (scale 0.999 speed)
                                                      <| scale 0.01
                                                      <| direction waypoint position
    newSpeed      = withDefault speed maybeSpeed
  in
    { particle | speed= newSpeed }

limitSpeed : Float -> Vec2 -> Vec2
limitSpeed max speed =
  case (Math.Vector2.length speed > max) of
    True  -> speed |> normalize |> scale max
    False -> speed

updateParticlesPosition : List Particle -> List Particle
updateParticlesPosition particles = List.map updateParticlePosition particles

updateParticlePosition : Particle -> Particle
updateParticlePosition ({position, speed} as particle) =
  { particle | position= add position speed }


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (second / 200) Tick


-- VIEW

view : Model -> Html Msg
view ({ particles, waypoints } as model) =
  let
    particleViews = List.map particleView particles
    waypointViews = List.map waypointView waypoints
  in
    svg [ viewBox "0 0 500 500", width "500px", height "500px" ]
      (waypointViews ++ particleViews)

particleView ({ position, waypointIndex } as particle) =
  let
    px = toString <| getX position
    py = toString <| getY position
  in
    circle [ cx px, cy py, r "2", fill "#000000" ] []

waypointView position =
  let
    px = toString <| getX position
    py = toString <| getY position
  in
    circle [ cx px, cy py, r "2", fill "#ff0000" ] []
