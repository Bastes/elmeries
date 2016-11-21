module Main exposing (..)

--where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import AnimationFrame
import Time exposing (second)
import Animation exposing (..)
import Color exposing (green, complement)


type alias Model =
    { style : Animation.State }


type Msg
    = Show
    | Hide
    | Animate Animation.Msg


styles =
    { open =
        [ left <| px 0.0
        , opacity 1.0
        , color (green)
        ]
    , closed =
        [ left <| px -350.0
        , opacity 0.0
        , color (green)
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Show ->
            ( { model
                | style =
                    model.style
                        |> Animation.interrupt
                            [ Animation.toWith
                                (easing
                                    { duration = (0.5 * second)
                                    , ease = (\x -> x)
                                    }
                                )
                                styles.open
                            ]
              }
            , Cmd.none
            )

        Hide ->
            ( { model
                | style =
                    model.style
                        |> Animation.interrupt
                            [ Animation.toWith
                                (easing
                                    { duration = (0.5 * second)
                                    , ease = (\x -> x)
                                    }
                                )
                                styles.closed
                            ]
              }
            , Cmd.none
            )

        Animate animMsg ->
            ( { model | style = Animation.update animMsg model.style }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div
        [ onMouseEnter Show
        , onMouseLeave Hide
        , Html.Attributes.style
            [ ( "position", "absolute" )
            , ( "left", "0px" )
            , ( "top", "0px" )
            , ( "width", "350px" )
            , ( "height", "100%" )
            , ( "border", "2px dashed #AAA" )
            ]
        ]
        [ h1 [ Html.Attributes.style [ ( "padding", "25px" ) ] ]
            [ text "Hover here to see menu!" ]
        , div
            ([ Html.Attributes.style
                [ ( "position", "absolute" )
                , ( "top", "-2px" )
                , ( "margin-left", "-2px" )
                , ( "padding", "25px" )
                , ( "width", "300px" )
                , ( "height", "100%" )
                , ( "background-color", "rgb(58,40,69)" )
                , ( "color", "white" )
                , ( "border", "2px solid rgb(58,40,69)" )
                ]
             ]
                ++ (Animation.render model.style)
            )
            [ h1 [] [ text "Hidden Menu" ]
            , ul []
                [ li [] [ text "Some things" ]
                , li [] [ text "in a list" ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate [ model.style ]


init : ( Model, Cmd Msg )
init =
    ( { style = Animation.style styles.closed }
    , Cmd.none
    )


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
