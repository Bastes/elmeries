import Html exposing (Html)
import Html.App as App
import Svg exposing (svg, rect)
import Svg.Attributes exposing (viewBox, width, height, x, y, fill, style)
import Svg.Events     exposing (onClick, onMouseDown, onMouseOver, onMouseUp)
import Window
import Task
import Time exposing (Time, second)
import List exposing (indexedMap, repeat, map, filterMap, concat, take)
import Maybe exposing (withDefault)

import GameOfLife exposing (..)

main = App.program
  { init          = init
  , view          = view
  , update        = update
  , subscriptions = subscriptions
  }


-- MODEL

type alias ScreenSize = { width: Int, height: Int }
type alias Model = { world:    World
                   , screen:   ScreenSize
                   , pause:    Bool
                   , dragging: Maybe Cell
                   }

init : (Model, Cmd Msg)
init = ( { world=    [[]]
         , screen=   { width= 0, height= 0 }
         , pause=    True
         , dragging= Nothing
         }
       , Task.perform (\_ -> WindowResize { width= 0, height= 0 }) (\dimentions -> WindowInit dimentions) Window.size
       )


-- UPDATE

type Msg = Tick Time
         | TogglePlay
         | SlideStart Int Int Cell
         | SlideHover Int Int Cell
         | SlideStop
         | WindowInit   ScreenSize
         | WindowResize ScreenSize

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ world, pause, dragging } as model) =
  case msg of
    Tick _ -> if pause then (model, Cmd.none) else ({ model | world = step world }, Cmd.none)
    SlideStart y x c -> update (SlideHover y x (invertCell c)) { model | dragging= Just (invertCell c) }
    SlideHover y x c -> ({ model | world= setCell y x (withDefault c dragging) world }, Cmd.none)
    SlideStop  -> ({ model | dragging= Nothing }, Cmd.none)
    TogglePlay -> ({ model | pause= not pause }, Cmd.none)
    WindowInit   screen -> ({ model | screen= screen, world= windowInitWorld screen }, Cmd.none)
    WindowResize screen -> ({ model | screen= screen }, Cmd.none)

setCell : Int -> Int -> Cell -> World -> World
setCell y x c = indexedMap (\cy -> indexedMap (\cx cc -> if (cy /= y || cx /= x) then cc else c))

invertCell : Cell -> Cell
invertCell c =
    case c of
      Dead -> Live
      Live -> Dead

windowInitWorld : ScreenSize -> World
windowInitWorld screen =
  let
    worldWidth  = screen.width  // cellWidth
    worldHeight = screen.height // cellWidth
  in
    repeat worldHeight (repeat worldWidth Dead)

windowResizeWorld : ScreenSize -> World -> World
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

view : Model -> Html Msg
view { screen, world, pause } =
  let
    svgViewBox  = viewBox <| "0 0 " ++ (toString screen.width) ++ " " ++ (toString screen.height)
    svgStyle    = style "cursor: pointer; position: fixed; top: 0; left: 0; width: 100%; height: 100%;"
  in
    svg [svgViewBox, svgStyle] ((worldView world) ++ [pauseToggleButton screen pause])

pauseToggleButton : ScreenSize -> Bool -> Html Msg
pauseToggleButton screen pause =
  let
    fillColor = if pause then "#ff0000" else "#00ff00"
  in
    rect [ x (toPix (screen.width  - (cellWidth * 2)))
         , y (toPix (screen.height - (cellWidth * 2)))
         , width  (toPix (cellWidth * 2))
         , height (toPix (cellWidth * 2))
         , fill fillColor
         , onClick TogglePlay
         ] []


worldView : World -> List (Html Msg)
worldView world = world |> indexedMap (\y -> indexedMap (\x c-> cellView y x c))
                        |> concat

cellView : Int -> Int -> Cell -> Html Msg
cellView cy cx c =
  let
    fillColor = case c of
      Dead -> "#ffffff"
      Live -> "#000000"
  in
    rect [ x (toPix (cx * cellWidth))
         , y (toPix (cy * cellWidth))
         , width  (toPix cellWidth)
         , height (toPix cellWidth)
         , fill fillColor
         , onMouseDown (SlideStart cy cx c)
         , onMouseOver (SlideHover cy cx c)
         , onMouseUp   (SlideStop)
         ]
         []

toPix : Int -> String
toPix n = (toString n) ++ "px"

cellWidth = 20
