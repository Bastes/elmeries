module Board exposing (Model, Msg(..), init, update, subscriptions, view)

import Html exposing (Html)
import Svg exposing (svg, rect)
import Svg.Attributes exposing (viewBox, style, width, height, x, y, fill)
import Svg.Events exposing (onMouseDown, onMouseOver, onMouseUp)
import List exposing (concat, indexedMap, head, filterMap)
import Maybe exposing (withDefault)
import GameOfLife exposing (World, Cell(..))
import Json.Decode as Json
import VirtualDom
import Debug


-- MODEL


type alias Width =
    Int


type alias Height =
    Int


type alias Dimensions =
    ( Height, Width )


type alias Model =
    { dimensions : Dimensions
    , dragging : Maybe Cell
    }


init : ( Model, Cmd msg )
init =
    ( { dimensions = ( 0, 0 )
      , dragging = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type alias Position =
    { y : Int, x : Int }


type Msg
    = MouseMove Position
    | MouseDown Position
    | MouseUp Position


update : Msg -> Model -> ( Model, Cmd Msg, String )
update msg model =
    case msg of
        MouseMove pos ->
            ( model, Cmd.none, "mousemove pos: " ++ (toString pos) )

        MouseDown pos ->
            ( model, Cmd.none, "mousedown pos: " ++ (toString pos) )

        MouseUp pos ->
            ( model, Cmd.none, "mouseup pos: " ++ (toString pos) )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : World -> Model -> Html Msg
view world { dimensions } =
    let
        ( height, width ) =
            dimensions

        boardHeight =
            List.length world |> toString

        boardWidth =
            List.head world |> withDefault [] |> List.length |> toString

        svgViewBox =
            viewBox <| "0 0 " ++ boardWidth ++ " " ++ boardHeight

        svgStyle =
            style <| "cursor: pointer; width: " ++ (toString width) ++ "px; height: " ++ (toString height) ++ "px;"

        cells =
            world
                |> indexedMap2 cellView
                |> concat
                |> filterMap identity
    in
        svg
            [ svgViewBox
            , svgStyle
            , onMouseWithPosition "mousemove" MouseMove
            , onMouseWithPosition "mousedown" MouseDown
            , onMouseWithPosition "mouseup" MouseUp
            ]
            cells


onMouseWithPosition eventType msg =
    VirtualDom.on eventType <| Json.map msg offsetPosition


offsetPosition : Json.Decoder Position
offsetPosition =
    Json.map2 Position (Json.field "offsetY" Json.int) (Json.field "offsetX" Json.int)


indexedMap2 : (Int -> Int -> a -> b) -> List (List a) -> List (List b)
indexedMap2 f =
    indexedMap (\y -> indexedMap (\x a -> f y x a))


cellView : Int -> Int -> Cell -> Maybe (Html Msg)
cellView cy cx c =
    case c of
        Dead ->
            Nothing

        Live ->
            Just <|
                rect
                    [ x (toString cx)
                    , y (toString cy)
                    , width "1"
                    , height "1"
                    , fill "#000000"
                    ]
                    []
