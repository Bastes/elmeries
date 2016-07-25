import Die
import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import List exposing (unzip, repeat, indexedMap)

main =
  App.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { dice: List IndexedDie
  , uid:  Int
  }

type alias IndexedDie =
  { id:    Int
  , model: Die.Model
  }

init : (Model, Cmd Msg)
init =
  let
    number = 3
    (dice, cmds) = unzip <| repeat number Die.init
  in
    ( { dice = indexedMap (\uid d -> { id = uid, model = d }) dice
      , uid  = number
      }
    , Cmd.batch <| indexedMap (\uid c -> Cmd.map (Trigger (uid + 1)) c) cmds
    )

-- UPDATE

type Msg = Add
         | Remove
         | RollAll
         | Trigger Int Die.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update message ({uid, dice} as model) =
  case message of
    Add ->
      let
        (die, cmd) = Die.init
      in
        ( { model
          | dice = dice ++ [ IndexedDie uid die ]
          , uid  = uid + 1
          }
        , Cmd.batch [Cmd.map (Trigger uid) cmd]
        )
    Remove ->
      ( { model
        | dice = List.drop 1 dice
        , uid  = uid - 1
        }
        , Cmd.none
      )
    RollAll ->
      let
        results      = List.map updateAllHelp model.dice
        (dice, cmds) = List.unzip results
      in
        ( { model | dice = dice }
        , Cmd.batch cmds
        )
    Trigger id msg ->
      let
        results      = List.map (updateHelp id msg) model.dice
        (dice, cmds) = List.unzip results
      in
        ( { model | dice = dice }
        , Cmd.batch cmds
        )

updateAllHelp : IndexedDie -> (IndexedDie, Cmd Msg)
updateAllHelp {id, model} =
  let
    (die, cmd) = Die.update Die.Roll model
  in
    (IndexedDie id die, Cmd.map (Trigger id) cmd)

updateHelp : Int -> Die.Msg -> IndexedDie -> (IndexedDie, Cmd Msg)
updateHelp targetId msg {id, model} =
  let
    (die, cmd) = if targetId == id then Die.update msg model else (model, Cmd.none)
  in
    (IndexedDie id die, Cmd.map (Trigger id) cmd)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch (subscriptionsToDice model.dice)

subscriptionsToDice : List IndexedDie -> List (Sub Msg)
subscriptionsToDice = List.map subscriptionsHelp

subscriptionsHelp : IndexedDie -> Sub Msg
subscriptionsHelp {id, model} =
  Sub.map (Trigger id) (Die.subscriptions model)

-- VIEW

view : Model -> Html Msg
view model =
  let
    add     = button [onClick Add]     [text "Add"]
    remove  = button [onClick Remove]  [text "Remove"]
    rollall = button [onClick RollAll] [text "RollAll"]
    dice    = List.map viewIndexedDie model.dice
  in
    div [] ([rollall, add, remove] ++ dice)

viewIndexedDie : IndexedDie -> Html Msg
viewIndexedDie {id, model} =
  App.map (Trigger id) (Die.view model)
