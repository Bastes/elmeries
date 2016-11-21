module NextButton exposing (nextButton)

import String
import Html exposing (Html)
import Svg exposing (svg, rect, polygon, g)
import Svg.Attributes exposing (viewBox, style, width, height, x, y, fill, points)
import Svg.Events exposing (onClick)


type alias Active =
    Bool


type alias Width =
    Int


type alias Y =
    Int


type alias X =
    Int


nextButton : Width -> Active -> msg -> Html msg
nextButton iWidth active action =
    let
        sWidth =
            toString iWidth

        svgViewBox =
            viewBox <| "0 0 10 10"

        cursorStyle =
            case active of
                False ->
                    ""

                True ->
                    "cursor: pointer; "

        svgStyle =
            style <| cursorStyle ++ "width: " ++ sWidth ++ "px; height: " ++ sWidth ++ "px;"

        frame =
            rect [ y "0", x "0", height sWidth, width sWidth, fill "#777777" ] []

        fillColor =
            case active of
                False ->
                    "#bbbbbb"

                True ->
                    "#ffffff"

        playSymbol =
            [ polygon [ points "2,3 2,7 6,5", fill fillColor ] []
            , polygon [ points "4,3 4,7 8,5", fill fillColor ] []
            ]

        actions =
            case active of
                False ->
                    []

                True ->
                    [ onClick action ]
    in
        svg [ svgViewBox, svgStyle ] [ g actions (frame :: playSymbol) ]
