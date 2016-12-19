module Board exposing (Model, Msg(..), init, update, subscriptions, view)

import Html exposing (Html)
import Svg exposing (svg, rect)
import Svg.Attributes exposing (viewBox, style, width, height, x, y, fill)
import Svg.Events exposing (onMouseDown, onMouseOver, onMouseUp)
import List exposing (concat, indexedMap, head)
import Maybe exposing (withDefault, andThen)
import GameOfLife exposing (World, Cell(..), toggleCell, setCell, step)
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
    { worlds : List World
    , dimensions : Dimensions
    , dragging : Maybe Cell
    }


init : ( Model, Cmd msg )
init =
    ( { worlds = [ [] ]
      , dimensions = ( 0, 0 )
      , dragging = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type alias Position =
    { y : Int, x : Int }


type Msg
    = SlideStart Int Int Cell
    | SlideHover Int Int Cell
    | SlideStop
    | Step
    | InitWith World
    | MouseMove Position
    | MouseDown Position
    | MouseUp Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ worlds } as model) =
    case msg of
        SlideStart y x c ->
            let
                draggingCell =
                    toggleCell c

                dragging =
                    Just draggingCell

                world =
                    worlds |> updateHeadWith (setCell y x draggingCell)
            in
                ( { model | dragging = dragging, worlds = [ world ] }
                , Cmd.none
                )

        SlideHover y x c ->
            let
                world =
                    worlds |> updateHeadWith (setCell y x (withDefault c model.dragging))
            in
                ( { model | worlds = [ world ] }
                , Cmd.none
                )

        SlideStop ->
            ( { model | dragging = Nothing }
            , Cmd.none
            )

        Step ->
            let
                world =
                    worlds |> updateHeadWith step
            in
                ( { model | worlds = world :: model.worlds }
                , Cmd.none
                )

        InitWith world ->
            ( { model | worlds = [ world ] }
            , Cmd.none
            )

        MouseMove pos ->
            always ( model, Cmd.none ) (Debug.log "mousemove pos: " pos)

        MouseDown pos ->
            always ( model, Cmd.none ) (Debug.log "mousedown pos: " pos)

        MouseUp pos ->
            always ( model, Cmd.none ) (Debug.log "mouseup pos: " pos)


updateHeadWith : (World -> World) -> List World -> World
updateHeadWith update worlds =
    head worlds |> andThen (Just << update) |> withDefault []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view { worlds, dimensions } =
    let
        world =
            head worlds |> withDefault []

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


indexedMap2 : (Int -> Int -> a -> b) -> List (List a) -> List (List b)
indexedMap2 f =
    indexedMap (\y -> indexedMap (\x a -> f y x a))


offsetPosition : Json.Decoder Position
offsetPosition =
    Json.map2 Position (Json.field "offsetY" Json.int) (Json.field "offsetX" Json.int)


cellView : Int -> Int -> Cell -> Html Msg
cellView cy cx c =
    let
        fillColor =
            case c of
                Dead ->
                    "#ffffff"

                Live ->
                    "#000000"
    in
        rect
            [ x (toString cx)
            , y (toString cy)
            , width "1"
            , height "1"
            , fill fillColor
            , onMouseDown (SlideStart cy cx c)
            , onMouseOver (SlideHover cy cx c)
            , onMouseUp (SlideStop)
            ]
            []
