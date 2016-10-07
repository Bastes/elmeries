import Html exposing (Html)
import Html.App as App
import Svg exposing (svg, circle, node, line, rect)
import Svg.Attributes exposing (viewBox, width, height, cx, cy, x, y, x1, x2, y1, y2, r, stroke, fill, style)
import Window
import Task
import Random
import Time exposing (Time, second)
import Math.Vector2 exposing (Vec2, vec2, add, sub, getX, getY, normalize, scale, distance, direction)
import Array exposing (get, fromList)
import Maybe exposing (withDefault, andThen)
import Color exposing (hsla)
import Color.Convert exposing (colorToCssHsla)

main = App.program
  { init          = init
  , view          = view
  , update        = update
  , subscriptions = subscriptions
  }


-- MODEL

type alias Particle = { position: Vec2
                      , speed:    Vec2
                      , birth:    Time
                      }
type alias Model    = { particles: List Particle
                      , origin:    Vec2
                      , screen:    Vec2
                      }

init : (Model, Cmd Msg)
init = ( { particles= [] , origin= vec2 0 0, screen= vec2 0 0 }
       , Task.perform (\_ -> WindowResize { width= 0, height= 0 }) (\dimentions -> WindowInit dimentions) Window.size
       )


-- TOOLING

rotate : Float -> Vec2 -> Vec2
rotate a v =
  let
    (x, y) = (getX v, getY v)
    (c, s) = (cos a, sin a)
  in
     vec2 (c * x - s * y) (s * x + c * y)


rotateAround : Vec2 -> Float -> Vec2 -> Vec2
rotateAround c a = add c << rotate a << sub c


-- UPDATE

type Msg = Tick Time
         | Spawn Time Int
         | WindowInit   { width: Int, height: Int }
         | WindowResize { width: Int, height: Int }

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ origin, particles } as model) =
  case msg of
    Spawn time angle ->
      let
        newParticle = { position= origin, speed= rotate (pi * (toFloat angle) / 180) (vec2 0 -0.01), birth= time }
      in
        ( { model | particles= particles ++ [newParticle] }, Cmd.none )
    Tick time ->
      let
        newParticles = particles
                    |> trashOldParticles
                    |> moveParticles
        newCmd =
          if List.all (olderThan (time - second / 10)) particles then
            Random.generate (Spawn time) (Random.int -90 90)
          else
            Cmd.none
      in
        ( { model | particles= newParticles }, newCmd )
    WindowResize { width, height } ->
      let
        w = toFloat width
        h = toFloat height
      in
        ( { model | origin= vec2 (0.5 * w) (0.5 * h)
                  , screen= vec2 (toFloat width) (toFloat height)
          }
        , Cmd.none
        )
    WindowInit { width, height } ->
      let
        w = toFloat width
        h = toFloat height
      in
        ( { particles= []
          , origin= vec2 (0.5 * w) (0.5 * h)
          , screen= vec2 w h
          }
        , Cmd.none
        )

moveParticles : List Particle -> List Particle
moveParticles particles =
  List.map moveParticle particles

moveParticle : Particle -> Particle
moveParticle ({ position, speed } as particle) =
  { particle | position = add position speed
             , speed    = normalize <| add speed (vec2 0 -0.003)
  }

trashOldParticles : List Particle -> List Particle
trashOldParticles particles =
  if List.length particles >= 100 then
    withDefault [] (List.tail particles)
  else
    particles

olderThan : Time -> Particle -> Bool
olderThan time { birth } = time > birth

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [ Time.every (second / 60) Tick
            , Window.resizes WindowResize
            ]


-- VIEW

view : Model -> Html Msg
view {particles, origin, screen} =
  let
    particleViews = List.map (particleView origin screen  0 "50") particles
                 ++ List.map (particleView origin screen 10 "40") particles
                 ++ List.map (particleView origin screen 20 "30") particles
                 ++ List.map (particleView origin screen 30 "20") particles
                 ++ List.map (particleView origin screen 40 "10") particles
    px = toString <| getX screen
    py = toString <| getY screen
    svgViewBox = viewBox <| "0 0 " ++ px ++ " " ++ py
    svgStyle   = style "position: fixed; top: 0; left: 0; width: 100%; height: 100%;"
  in
    svg [ svgViewBox, svgStyle ]
        ((rect [x "0", y "0", width px, height py, fill "#000000"] []) :: particleViews)

particleView origin screen hue radius {position, speed} =
  let
    strength = 1 - (distance origin position) / (getY screen - getY origin)
    px = toString <| getX position
    py = toString <| getY position
  in
    circle [ cx px, cy py, r radius, fill (colorToCssHsla (hsla (degrees hue) strength (0.5 * strength) (0.5 * strength))) ] []
