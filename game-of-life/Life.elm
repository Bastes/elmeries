module Main exposing (..)

import Html exposing (Html, div)
import Svg exposing (svg, rect, g, polygon)
import Svg.Attributes exposing (viewBox, width, height, x, y, fill, style, points)
import Svg.Events exposing (onClick, onMouseDown, onMouseOver, onMouseUp)
import Window
import Task
import Time exposing (Time, second)
import List exposing (indexedMap, repeat, map, filterMap, concat, take, intersperse, head, tail)
import Maybe exposing (withDefault, andThen)
import Random
import String
import GameOfLife exposing (..)
import Board
import GolButtons exposing (pauseButton, nextButton, prevButton)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Height =
    Int


type alias Width =
    Int


type alias Dimensions =
    { height : Height
    , width : Width
    }


type alias Model =
    { worlds : List World
    , board : Board.Model
    , screen : Dimensions
    , pause : Bool
    }


init : ( Model, Cmd Msg )
init =
    let
        ( board, boardCmd ) =
            Board.init

        windowInit =
            Task.perform
                (\dimentions -> WindowInit dimentions)
                Window.size
    in
        ( { worlds = [ [] ]
          , board = board
          , screen = { width = 0, height = 0 }
          , pause = True
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
    | PrevFrame
    | NextFrame
    | BoardMsg Board.Msg
    | WindowInit Dimensions
    | WindowResize Dimensions
    | WorldInit World


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ worlds, board, pause } as model) =
    case msg of
        Tick _ ->
            let
                newWorld =
                    head worlds |> withDefault [] |> step
            in
                ( { model | worlds = newWorld :: worlds }
                , Cmd.none
                )

        BoardMsg msg ->
            let
                ( newBoard, boardCmds ) =
                    Board.update msg board
            in
                ( { model | board = newBoard }
                , Cmd.map BoardMsg boardCmds
                )

        TogglePlay ->
            ( { model | pause = not pause }
            , Cmd.none
            )

        PrevFrame ->
            let
                newWorlds =
                    tail worlds |> withDefault worlds
            in
                ( { model | worlds = newWorlds }
                , Cmd.none
                )

        NextFrame ->
            let
                newWorld =
                    head worlds |> withDefault [] |> step
            in
                ( { model | worlds = newWorld :: worlds }
                , Cmd.none
                )

        WindowInit screen ->
            ( { model
                | screen = screen
                , board = { board | dimensions = ( screen.height - controlsHeight - cellWidth, screen.width ) }
              }
            , generateRandomWorld screen
            )

        WindowResize screen ->
            ( { model
                | screen = screen
                , board = { board | dimensions = ( screen.height - controlsHeight - cellWidth, screen.width ) }
              }
            , Cmd.none
            )

        WorldInit world ->
            ( { model | worlds = [ world ] }
            , Cmd.none
            )


generateRandomWorld : Dimensions -> Cmd Msg
generateRandomWorld screen =
    let
        deadOrLive =
            (\n ->
                if n == 1 then
                    Live
                else
                    Dead
            )

        randomCell =
            Random.map deadOrLive (Random.int 0 1)

        randomLine =
            Random.list ((screen.width // cellWidth)) randomCell

        randomWorld =
            Random.list ((screen.height // cellWidth) - 3) randomLine
    in
        Random.generate WorldInit randomWorld



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { board, pause } =
    let
        tick =
            if pause then
                Sub.none
            else
                Time.every (second / 10) Tick
    in
        Sub.batch
            [ tick
            , Window.resizes WindowResize
            , Sub.map BoardMsg (Board.subscriptions board)
            ]



-- VIEW


cellWidth =
    20


controlsHeight =
    cellWidth * 2


view : Model -> Html Msg
view { worlds, screen, board, pause } =
    let
        world =
            head worlds |> withDefault []
    in
        div
            []
            [ controls worlds controlsHeight pause
            , Html.map BoardMsg (Board.view world board)
            ]


controls : List World -> Height -> Bool -> Html Msg
controls worlds height pause =
    let
        hasPrevious =
            pause && (List.length worlds) > 1
    in
        div
            []
            [ prevButton height hasPrevious PrevFrame
            , pauseButton height pause TogglePlay
            , nextButton height pause NextFrame
            ]
