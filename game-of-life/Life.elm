import Html exposing (Html, div)
import Html.App as App
import Svg exposing (svg, rect, g, polygon)
import Svg.Attributes exposing (viewBox, width, height, x, y, fill, style, points)
import Svg.Events     exposing (onClick, onMouseDown, onMouseOver, onMouseUp)
import Window
import Task
import Time exposing (Time, second)
import List exposing (indexedMap, repeat, map, filterMap, concat, take, intersperse)
import Maybe exposing (withDefault)
import Random
import String

import GameOfLife exposing (..)

import PauseButton exposing (pauseButton)

main = App.program
  { init          = init
  , view          = view
  , update        = update
  , subscriptions = subscriptions
  }


-- MODEL

type alias Height = Int
type alias Width  = Int

type alias Dimensions =
  { height: Height
  , width:  Width
  }

type alias Model =
  { world:    World
  , screen:   Dimensions
  , pause:    Bool
  , dragging: Maybe Cell
  }

init : (Model, Cmd Msg)
init =
  ( { world=    [[]]
    , screen=   { width= 0, height= 0 }
    , pause=    True
    , dragging= Nothing
    }
  , Task.perform
    (\_          -> WindowInit { width= 0, height= 0 })
    (\dimentions -> WindowInit dimentions)
    Window.size
  )


-- UPDATE

type Msg
    = Tick Time
    | TogglePlay
    | SlideStart Int Int Cell
    | SlideHover Int Int Cell
    | SlideStop
    | WindowInit   Dimensions
    | WindowResize Dimensions
    | WorldInit    World

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ world, pause, dragging } as model) =
  case msg of
    Tick _ ->
      if pause then
        (model, Cmd.none)
      else
        ({ model | world = step world }, Cmd.none)
    SlideStart y x c ->
      let
        draggingCell = toggleCell c
      in
        ( { model
          | dragging= Just draggingCell
          , world=    setCell y x draggingCell world
          }
        , Cmd.none
        )
    SlideHover y x c ->
      ( { model | world= setCell y x (withDefault c dragging) world }
      , Cmd.none
      )
    SlideStop ->
      ( { model | dragging= Nothing }
      , Cmd.none
      )
    TogglePlay ->
      ( { model | pause= not pause }
      , Cmd.none
      )
    WindowInit screen ->
      ( { model | screen= screen }
      , generateRandomWorld screen
      )
    WindowResize screen ->
      ( { model | screen= screen }
      , Cmd.none
      )
    WorldInit w ->
      ( { model | world= w }
      , Cmd.none
      )

generateRandomWorld : Dimensions -> Cmd Msg
generateRandomWorld screen =
  let
    deadOrLive  = (\n -> if n == 1 then Live else Dead)
    randomCell  = Random.map deadOrLive (Random.int 0 1)
    randomLine  = Random.list (screen.width  // cellWidth) randomCell
    randomWorld = Random.list (screen.height // cellWidth) randomLine
  in
    Random.generate WorldInit randomWorld

windowResizeWorld : Dimensions -> World -> World
windowResizeWorld screen world =
  let
    newWorldWidth  = screen.width  // cellWidth
    newWorldHeight = screen.height // cellWidth
  in
    world |> take newWorldHeight
          |> map (take newWorldWidth)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [ Time.every (second / 10) Tick
            , Window.resizes WindowResize
            ]

-- VIEW

cellWidth = 20

view : Model -> Html Msg
view { screen, world, pause } =
  div
    []
    [ controls (cellWidth * 2) pause
    , worldView { screen | height= screen.height - (cellWidth * 3) } world
    ]

svgOf : Dimensions -> List (Html Msg) -> Html Msg
svgOf screen =
  let
    width  = (toString screen.width)
    height = (toString screen.height)
    svgViewBox  =
      viewBox <| "0 0 " ++ width ++ " " ++ height
    svgStyle    =
      style <| "cursor: pointer; width: " ++ width ++ "px; height: " ++ height ++ "px;"
  in
    svg [svgViewBox, svgStyle]

controls : Height -> Bool -> Html Msg
controls height pause =
  div
    []
    [ pauseButton height pause TogglePlay ]

indexedMap2 : (Int -> Int -> a -> b) -> List (List a) -> List (List b)
indexedMap2 f = indexedMap (\y -> indexedMap (\x a -> f y x a))

worldView : Dimensions -> World -> Html Msg
worldView screen world =
  let
    cells =
      world |> indexedMap2 cellView
            |> concat
  in
    svgOf screen cells

cellView : Int -> Int -> Cell -> Html Msg
cellView cy cx c =
  let
    fillColor = case c of
      Dead -> "#ffffff"
      Live -> "#000000"
  in
    rect
    [ x (toString (cx * cellWidth))
    , y (toString (cy * cellWidth))
    , width  (toString cellWidth)
    , height (toString cellWidth)
    , fill fillColor
    , onMouseDown (SlideStart cy cx c)
    , onMouseOver (SlideHover cy cx c)
    , onMouseUp   (SlideStop)
    ]
    []
