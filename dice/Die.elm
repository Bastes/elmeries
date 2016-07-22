module Die exposing (Model, Msg(Roll), init, update, subscriptions, view)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Random

-- Model

type alias Model = { dieFace : Int }

init : (Model, Cmd Msg)
init = (Model 1, Cmd.none)

-- UPDATE

type Msg = Roll
         | NewFace Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      (model, Random.generate NewFace (Random.int 1 6))

    NewFace newFace ->
      (Model newFace, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div [ dieStyle ]
    [ face model
    , button [ onClick Roll ] [ Html.text "Roll" ]
    ]

dieRadius      = 7
dieOffset      = 25
dieWidth       = 100
dieTop         = dieOffset
dieBottom      = dieWidth - dieOffset
dieLeft        = dieTop
dieRight       = dieBottom
dieMiddle      = dieWidth / 2
dieBodyOffset  = (dieOffset - dieRadius * 3)
dieBodyOffsetS = toString dieBodyOffset
dieBodyWidthS  = toString (dieWidth - dieBodyOffset * 2)
dieBodyRadiusS = toString (dieRadius * 2)
dieBody        = rect [x dieBodyOffsetS, y dieBodyOffsetS, width dieBodyWidthS, height dieBodyWidthS, rx dieBodyRadiusS, ry dieBodyRadiusS, fill "#ffffff", stroke "#000000"] []
dot x y       = circle [ cx (toString x), cy (toString y), r (toString dieRadius), fill "#000000" ] []

face : Model -> Html Msg
face { dieFace } =
  let
    circles =
      case dieFace of
        1 ->
          [ dot dieMiddle dieMiddle ]
        2 ->
          [ dot dieLeft  dieTop
          , dot dieRight dieBottom
          ]
        3 ->
          [ dot dieLeft   dieTop
          , dot dieMiddle dieMiddle
          , dot dieRight  dieBottom
          ]
        4 ->
          [ dot dieLeft  dieTop
          , dot dieRight dieTop
          , dot dieLeft  dieBottom
          , dot dieRight dieBottom
          ]
        5 ->
          [ dot dieLeft   dieTop
          , dot dieRight  dieTop
          , dot dieMiddle dieMiddle
          , dot dieLeft   dieBottom
          , dot dieRight  dieBottom
          ]
        6 ->
          [ dot dieLeft  dieTop
          , dot dieRight dieTop
          , dot dieLeft  dieMiddle
          , dot dieRight dieMiddle
          , dot dieLeft  dieBottom
          , dot dieRight dieBottom
          ]
        _ ->
          []
  in
    Svg.svg [ viewBox "0 0 100 100", Html.Attributes.style [("display", "inline-block"), ("width", "100px"), ("height", "100px")] ]
      ([dieBody] ++ circles)

dieStyle : Html.Attribute msg
dieStyle =
  Html.Attributes.style
    [ ("display", "inline-block")
    , ("width", "100px")
    , ("text-align", "center")
    ]
