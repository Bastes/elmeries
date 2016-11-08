module PauseButton exposing (pauseButton)

import String
import List           exposing (map, intersperse, reverse)
import Html           exposing (Html)
import Svg            exposing (svg, rect, polygon, g)
import Svg.Attributes exposing (viewBox, style, width, height, x, y, fill, points)
import Svg.Events     exposing (onClick)

type alias Pause = Bool
type alias Width = Int
type alias Y     = Int
type alias X     = Int

pauseButton : Width -> Pause -> msg -> Html msg
pauseButton iWidth pause action =
  let
    sWidth =
      toString iWidth
    svgViewBox  =
      viewBox <| "0 0 10 10"
    svgStyle    =
      style <| "cursor: pointer; width: " ++ sWidth ++ "px; height: " ++ sWidth ++ "px;"
    frame =
      rect [ y "0", x "0", height sWidth, width sWidth, fill "#777777" ] []
    pauseSymbol =
      [ rect [ y "2", x "2", height "6", width "2", fill "#ffffff" ] []
      , rect [ y "2", x "6", height "6", width "2", fill "#ffffff" ] []
      ]
    playSymbol =
      [ polygon [ points  "2,2 2,8 8,5", fill "#ffffff" ] [] ]
    symbol = case pause of
      False -> pauseSymbol
      True  -> playSymbol
  in
    svg [ svgViewBox, svgStyle ] [ g [onClick action] (frame :: symbol) ]
