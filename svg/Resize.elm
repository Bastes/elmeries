module Main exposing (..)

import Window
import Task
import Html exposing (Html)
import Svg exposing (svg, text, text_)
import Svg.Attributes exposing (viewBox, width, height, x, y, style)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    ( Int, Int )


defaultDimensions =
    ( 100, 100 )


init : ( Model, Cmd Msg )
init =
    ( defaultDimensions, Task.perform (\dimentions -> WindowResize dimentions) Window.size )



-- UPDATE


type Msg
    = WindowResize { width : Int, height : Int }
    | Fail


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize { width, height } ->
            ( ( width, height ), Cmd.none )

        Fail ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes WindowResize



-- VIEW


view : Model -> Html Msg
view ( w, h ) =
    svg [ viewBox <| "0 0 " ++ (toString w) ++ " " ++ (toString h), width "100%", height "100%", style "position: fixed; left: 0%; top: 0%;" ]
        [ text_ [ x "2", y "15" ] [ text ((toString w) ++ " " ++ (toString h)) ] ]
