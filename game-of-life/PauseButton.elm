module PauseButton exposing (pauseButton)

import String
import List           exposing (map, intersperse, reverse)
import Html           exposing (Html)
import Svg            exposing (rect, polygon, g)
import Svg.Attributes exposing (width, height, x, y, fill, points)
import Svg.Events     exposing (onClick)

type alias Pause  = Bool
type alias Width  = Int
type alias Y      = Int
type alias X      = Int

toPoint : (Y, X) -> String
toPoint (y, x) = (toString x) ++ "," ++ (toString y)

toPoints : List (Y, X) -> Svg.Attribute msg
toPoints = points << joinWith " " << map toPoint

joinWith : String -> List String -> String
joinWith s = String.concat << intersperse s

pauseButton : Y -> X -> Width -> Pause -> msg -> Html msg
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
        [ y (by + (bw * 2) // 10 |> toString)
        , x (bx + (bw * 2) // 10 |> toString)
        , width  ((bw * 2) // 10 |> toString)
        , height ((bw * 6) // 10 |> toString)
        , fill "#ffffff"
        ] []
        , rect
        [ y (by + (bw * 2) // 10 |> toString)
        , x (bx + (bw * 6) // 10 |> toString)
        , height ((bw * 6) // 10 |> toString)
        , width  ((bw * 2) // 10 |> toString)
        , fill "#ffffff"
        ] []
      ]
    playSymbol =
      [ polygon
        [ toPoints
          [ ( by + ((bw * 2) // 10)
            , bx + ((bw * 2) // 10)
            )
          , ( by + ((bw * 8) // 10)
            , bx + ((bw * 2) // 10)
            )
          , ( by + ((bw * 5) // 10)
            , bx + ((bw * 8) // 10)
            )
          ]
        , fill "#ffffff"
        ] []
      ]
    symbol = case pause of
      False -> pauseSymbol
      True  -> playSymbol
  in
    g [onClick action] (frame :: symbol)
