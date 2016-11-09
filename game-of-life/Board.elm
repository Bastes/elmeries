module Board exposing (Model, Msg(..), init, update, subscriptions, view)

import Html exposing (Html)
import Svg exposing (svg, rect)
import Svg.Attributes exposing (viewBox, style, width, height, x, y, fill)
import Svg.Events exposing (onMouseDown, onMouseOver, onMouseUp)
import List exposing (concat, indexedMap)
import Maybe exposing (withDefault)
import GameOfLife exposing (World, Cell(..), toggleCell, setCell)

-- MODEL

type alias Width  = Int
type alias Height = Int

type alias Dimensions = (Height, Width)

type alias Model =
  { world:      World
  , dimensions: Dimensions
  , dragging:   Maybe Cell
  }


init : (Model, Cmd msg)
init =
    ( { world      = []
      , dimensions = (0, 0)
      , dragging   = Nothing
      }
    , Cmd.none
    )


-- UPDATE

type Msg
    = SlideStart Int Int Cell
    | SlideHover Int Int Cell
    | SlideStop


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SlideStart y x c ->
      let
        draggingCell = toggleCell c
        dragging = Just draggingCell
        world = setCell y x draggingCell model.world
      in
        ( { model | dragging= dragging, world= world }
        , Cmd.none
        )
    SlideHover y x c ->
      ( { model
        | world= setCell y x (withDefault c model.dragging) model.world
        }
      , Cmd.none
      )
    SlideStop ->
      ( { model | dragging= Nothing }
      , Cmd.none
      )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


-- VIEW

view : Model -> Html Msg
view { world, dimensions } =
  let
    (height, width) =
      dimensions
    boardHeight =
      List.length world |> toString
    boardWidth =
      List.head world |> withDefault [] |> List.length |> toString
    svgViewBox =
      viewBox <| "0 0 " ++ boardWidth ++ " " ++ boardHeight
    svgStyle    =
      style <| "cursor: pointer; width: " ++ (toString width) ++ "px; height: " ++ (toString height) ++ "px;"
    cells =
      world |> indexedMap2 cellView
            |> concat
  in
    svg [svgViewBox, svgStyle] cells


indexedMap2 : (Int -> Int -> a -> b) -> List (List a) -> List (List b)
indexedMap2 f = indexedMap (\y -> indexedMap (\x a -> f y x a))


cellView : Int -> Int -> Cell -> Html Msg
cellView cy cx c =
  let
    fillColor = case c of
      Dead -> "#ffffff"
      Live -> "#000000"
  in
    rect
    [ x (toString cx)
    , y (toString cy)
    , width "1"
    , height "1"
    , fill fillColor
    , onMouseDown (SlideStart cy cx c)
    , onMouseOver (SlideHover cy cx c)
    , onMouseUp   (SlideStop)
    ]
    []
