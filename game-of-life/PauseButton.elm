module PauseButton exposing (pauseButton)

import String
import List           exposing (map, intersperse, reverse)
import Html           exposing (Html)
import Svg            exposing (rect, polygon, g)
import Svg.Attributes exposing (width, height, x, y, fill, points)
import Svg.Events     exposing (onClick)

type alias Width  = Int
type alias Y      = Int
type alias X      = Int

toPoints : List (List Int) -> Svg.Attribute msg
toPoints = points << joinWith " " << map (joinWith "," << map toString << reverse)

joinWith : String -> List String -> String
joinWith s = String.concat << intersperse s

pauseButton : Y -> X -> Width -> Bool -> msg -> Html msg
pauseButton by bx bw pause action =
  let
    frame =
      rect
      [ y      (by |> toString)
      , x      (bx |> toString)
      , height (bw |> toString)
      , width  (bw |> toString)
      , fill "#777777"
      ] []
    pauseSymbol =
      [ rect
        [ y (by + (bw * 20) // 100 |> toString)
        , x (bx + (bw * 20) // 100 |> toString)
        , width  ((bw * 20) // 100 |> toString)
        , height ((bw * 60) // 100 |> toString)
        , fill "#ffffff"
        ] []
        , rect
        [ y (by + (bw * 20) // 100 |> toString)
        , x (bx + (bw * 60) // 100 |> toString)
        , height ((bw * 60) // 100 |> toString)
        , width  ((bw * 20) // 100 |> toString)
        , fill "#ffffff"
        ] []
      ]
    playSymbol =
      [ polygon
        [ toPoints
          [ [ by + ((bw * 20) // 100)
            , bx + ((bw * 20) // 100)
            ]
          , [ by + ((bw * 80) // 100)
            , bx + ((bw * 20) // 100)
            ]
          , [ by + ((bw * 50) // 100)
            , bx + ((bw * 80) // 100)
            ]
          ]
        , fill "#ffffff"
        ] []
      ]
    symbol = case pause of
      False -> pauseSymbol
      True  -> playSymbol
  in
    g [onClick action] (frame :: symbol)
