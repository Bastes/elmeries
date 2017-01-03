module Board exposing (Model, Msg(..), init, update, subscriptions, view, cellWidth)

import Html exposing (Html)
import Svg exposing (svg, rect)
import Svg.Attributes exposing (viewBox, style, width, height, x, y, fill)
import Svg.Events exposing (onMouseDown, onMouseOver, onMouseUp)
import List exposing (concat, indexedMap, head, filterMap)
import Maybe exposing (withDefault)
import GameOfLife exposing (World, Cell(..), setCell, cellAt, toggleCell)
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


cellWidth : Int
cellWidth =
    20


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
    = MouseMove World Position
    | MouseDown World Position
    | MouseUp World Position


update : Msg -> Model -> ( Model, Cmd Msg, Maybe World )
update msg model =
    case msg of
        MouseMove world pos ->
            case model.dragging of
                Just dragging ->
                    let
                        newWorld =
                            setCell pos.y pos.x dragging world
                    in
                        ( model, Cmd.none, Just newWorld )

                Nothing ->
                    ( model, Cmd.none, Nothing )

        MouseDown world pos ->
            let
                dragging =
                    cellAt pos.y pos.x world |> toggleCell

                newWorld =
                    setCell pos.y pos.x dragging world
            in
                ( { model | dragging = Just dragging }, Cmd.none, Just newWorld )

        MouseUp world pos ->
            ( { model | dragging = Nothing }, Cmd.none, Nothing )



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

        svgViewBox =
            viewBox <| "0 0 " ++ (toString width) ++ " " ++ (toString height)

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
            , onMouseWithPosition cellWidth "mousemove" <| MouseMove world
            , onMouseWithPosition cellWidth "mousedown" <| MouseDown world
            , onMouseWithPosition cellWidth "mouseup" <| MouseUp world
            ]
            cells


onMouseWithPosition : Width -> String -> (Position -> Msg) -> VirtualDom.Property Msg
onMouseWithPosition width eventType msg =
    let
        toCoordinate =
            Json.map (\v -> v // width)

        decodeCoordinate =
            flip Json.field Json.int >> toCoordinate

        offsetPosition =
            Json.map2
                Position
                (decodeCoordinate "offsetY")
                (decodeCoordinate "offsetX")
    in
        VirtualDom.on eventType <| Json.map msg offsetPosition


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
                    [ x (cx * cellWidth |> toString)
                    , y (cy * cellWidth |> toString)
                    , width (cellWidth |> toString)
                    , height (cellWidth |> toString)
                    , fill "#000000"
                    ]
                    []
