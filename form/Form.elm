import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import String
import Char

main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , ready : Bool
  }


model : Model
model =
  Model "" "" "" False


-- UPDATE

type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Submit


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }

    Submit ->
      { model | ready = True }


-- VIEW

view : Model -> Html Msg
view model =
  Html.form [ onSubmit Submit ]
    [ input [ type' "text",     placeholder "Name",              onInput Name ]          []
    , input [ type' "password", placeholder "Password",          onInput Password ]      []
    , input [ type' "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , input [ type' "submit",   value "Submit" ] []
    , viewValidation model ]

message : String -> String -> Html Msg
message color message =
  div [ style [("color", color)] ] [ text message ]

validMessage : String -> Html Msg
validMessage =
  message "green"

errorMessage : String -> Html Msg
errorMessage =
  message "red"

validate : (Model -> Bool) -> String -> Model -> Maybe (Html Msg)
validate condition message model =
  if condition model then
     Nothing
  else
     Just (errorMessage message)

passwordLength : Model -> Bool
passwordLength model = String.length model.password > 7

passwordPattern : Model -> Bool
passwordPattern model =
  let checks = List.map String.any [Char.isUpper, Char.isLower, Char.isDigit]
  in  List.all ((|>) model.password) checks

passwordsMatch : Model -> Bool
passwordsMatch model = model.password == model.passwordAgain

viewValidation : Model -> Html Msg
viewValidation model =
  let
    errors =
      List.filterMap ((|>) model)
        [ validate passwordLength  "Password should be at least 8 characters long"
        , validate passwordPattern "Password must contain at least an uppercase char, a lowercase char and a digit"
        , validate passwordsMatch  "Passwords do not match!" ]
    result =
      if List.isEmpty errors then
        [validMessage "OK"]
      else
        errors
  in
    if model.ready then
      div [] result
    else
      div [] []
