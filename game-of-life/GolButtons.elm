module GolButtons exposing (pauseButton, nextButton, prevButton, randomWorldButton, emptyWorldButton)

import String
import Html exposing (Html)
import Svg exposing (svg, rect, polygon, g)
import Svg.Attributes exposing (viewBox, style, width, height, x, y, fill, points)
import Svg.Events exposing (onClick)


type alias Pause =
    Bool


type alias Active =
    Bool


type alias Width =
    Int


type alias Y =
    Int


type alias X =
    Int


pauseButton : Width -> Pause -> msg -> Html msg
pauseButton iWidth pause action =
    let
        sWidth =
            toString iWidth

        svgViewBox =
            viewBox <| "0 0 10 10"

        svgStyle =
            style <| "cursor: pointer; width: " ++ sWidth ++ "px; height: " ++ sWidth ++ "px;"

        pauseSymbol =
            [ rect [ y "2", x "2", height "6", width "2", fill "#ffffff" ] []
            , rect [ y "2", x "6", height "6", width "2", fill "#ffffff" ] []
            ]

        playSymbol =
            [ polygon [ points "2,2 2,8 8,5", fill "#ffffff" ] [] ]

        symbol =
            case pause of
                False ->
                    pauseSymbol

                True ->
                    playSymbol
    in
        svg [ svgViewBox, svgStyle ] [ g [ onClick action ] symbol ]


nextButton : Width -> Active -> msg -> Html msg
nextButton iWidth active action =
    let
        symbol =
            \fillColor ->
                [ polygon [ points "2,3 2,7 6,5", fill fillColor ] []
                , polygon [ points "4,3 4,7 8,5", fill fillColor ] []
                ]
    in
        button iWidth active symbol action


prevButton : Width -> Active -> msg -> Html msg
prevButton iWidth active action =
    let
        symbol =
            \fillColor ->
                [ polygon [ points "6,3 6,7 2,5", fill fillColor ] []
                , polygon [ points "8,3 8,7 4,5", fill fillColor ] []
                ]
    in
        button iWidth active symbol action


randomWorldButton : Width -> Active -> msg -> Html msg
randomWorldButton iWidth active action =
    let
        symbol =
            \fillColor ->
                [ polygon [ points "2,2 4,2 4,3 5,3 5,4 3,4 3,3 2,3", fill fillColor ] []
                , polygon [ points "5,2 7,2 7,4 6,4 6,3 5,3", fill fillColor ] []
                , polygon [ points "7,4 8,4 8,6 6,6 6,5 7,5", fill fillColor ] []
                , polygon [ points "3,5 5,5 5,6 4,6 4,8 2,8 2,7 3,7", fill fillColor ] []
                , polygon [ points "5,6 6,6 6,7 8,7 8,8 5,8", fill fillColor ] []
                ]
    in
        button iWidth active symbol action


emptyWorldButton : Width -> Active -> msg -> Html msg
emptyWorldButton iWidth active action =
    let
        symbol =
            \fillColor ->
                [ rect [ y "2", x "2", height "6", width "6", fill fillColor ] []
                ]
    in
        button iWidth active symbol action


button : Width -> Active -> (String -> List (Html msg)) -> msg -> Html msg
button iWidth active symbol action =
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

        fillColor =
            case active of
                False ->
                    "#bbbbbb"

                True ->
                    "#ffffff"

        actions =
            case active of
                False ->
                    []

                True ->
                    [ onClick action ]
    in
        svg [ svgViewBox, svgStyle ] [ g actions (symbol fillColor) ]
