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

import Board
import PauseButton exposing (pauseButton)
import NextButton exposing (nextButton)

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
  , width: Width
  }

type alias Model =
  { board: Board.Model
  , screen: Dimensions
  , pause: Bool
  }


init : (Model, Cmd Msg)
init =
  let
    (board, boardCmd) =
      Board.init
    windowInit =
      Task.perform
        (\_ -> WindowInit { width= 0, height= 0 })
        (\dimentions -> WindowInit dimentions)
        Window.size
  in
    ( { board= board
      , screen= { width= 0, height= 0 }
      , pause= True
      }
    , Cmd.batch
      [ windowInit
      , boardCmd
      ]
    )


-- UPDATE

type Msg
    = Tick Time
    | TogglePlay
    | NextFrame
    | BoardMsg Board.Msg
    | WindowInit Dimensions
    | WindowResize Dimensions
    | WorldInit World


update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ board, pause } as model) =
  case msg of
    Tick _ ->
      if pause then
        (model, Cmd.none)
      else
        ( { model | board= { board | world= step board.world } }
        , Cmd.none
        )
    BoardMsg msg ->
      let
        (board, boardCmds) = Board.update msg board
      in
        ( { model | board= board }
        , Cmd.map BoardMsg boardCmds
        )
    TogglePlay ->
      ( { model | pause= not pause }
      , Cmd.none
      )
    NextFrame ->
      ( { model | board= { board | world= step board.world } }
      , Cmd.none
      )
    WindowInit screen ->
      ( { model | screen= screen, board= { board | dimensions= (screen.height - (cellWidth * 3), screen.width) } }
      , generateRandomWorld screen
      )
    WindowResize screen ->
      ( { model | screen= screen, board= { board | dimensions= (screen.height - (cellWidth * 3), screen.width) } }

      , Cmd.none
      )
    WorldInit world ->
      ( { model | board= { board | world= world } }
      , Cmd.none
      )


generateRandomWorld : Dimensions -> Cmd Msg
generateRandomWorld screen =
  let
    deadOrLive  = (\n -> if n == 1 then Live else Dead)
    randomCell  = Random.map deadOrLive (Random.int 0 1)
    randomLine  = Random.list ((screen.width // cellWidth)) randomCell
    randomWorld = Random.list ((screen.height // cellWidth) - 3) randomLine
  in
    Random.generate WorldInit randomWorld


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [ Time.every (second / 10) Tick
            , Window.resizes WindowResize
            , Sub.map BoardMsg (Board.subscriptions model.board)
            ]


-- VIEW

cellWidth = 20


view : Model -> Html Msg
view { screen, board, pause } =
  div
    []
    [ controls (cellWidth * 2) pause
    , App.map BoardMsg (Board.view board)
    ]

controls : Height -> Bool -> Html Msg
controls height pause =
  div
    []
    [ pauseButton height pause TogglePlay
    , nextButton height pause NextFrame
    ]
