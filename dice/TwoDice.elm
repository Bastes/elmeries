import Die
import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)

main =
  App.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { top    : Die.Model
  , bottom : Die.Model
  }

init : (Model, Cmd Msg)
init =
  let
    (top,    topFx)    = Die.init
    (bottom, bottomFx) = Die.init
  in
    ( { top    = top
      , bottom = bottom
      }
    , Cmd.batch
      [ Cmd.map Top    topFx
      , Cmd.map Bottom bottomFx
      ]
    )

-- UPDATE

type Msg = RollAll
         | Top    Die.Msg
         | Bottom Die.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    RollAll ->
      let
        (top, topCmds)       = Die.update Die.Roll model.top
        (bottom, bottomCmds) = Die.update Die.Roll model.bottom
      in 
        ( { model | top = top, bottom = bottom }
        , Cmd.batch
          [ Cmd.map Top    topCmds
          , Cmd.map Bottom bottomCmds
          ]
        )
    Top msg ->
      let
        (top, topCmds) = Die.update msg model.top
      in
        ( { model | top = top }
        , Cmd.map Top topCmds
        )

    Bottom msg ->
      let
        (bottom, bottomCmds) = Die.update msg model.bottom
      in
        ( { model | bottom = bottom }
        , Cmd.map Bottom bottomCmds
        )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map Top    (Die.subscriptions model.top)
    , Sub.map Bottom (Die.subscriptions model.bottom)
    ]

-- VIEW

view : Model -> Html Msg

view model =
  div []
    [ App.map Top    (Die.view model.top)
    , App.map Bottom (Die.view model.bottom)
    , button [ onClick RollAll ] [ text "Roll both" ]
    ]
