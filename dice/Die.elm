module Die exposing (Model, Msg(Roll), init, update, subscriptions, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
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
    [ h1 [] [ text (toString model.dieFace) ]
    , button [ onClick Roll ] [ text "Roll" ]
    ]

dieStyle : Attribute msg
dieStyle =
  style
    [ ("display", "inline-block")
    , ("width", "50px")
    , ("text-align", "center")
    ]
