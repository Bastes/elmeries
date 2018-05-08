module Main exposing (..)

import List.Extra
import Maybe.Extra
import Html exposing (Html)
import Svg exposing (svg, circle, node, line, radialGradient, rect, stop, defs)
import Svg.Attributes exposing (viewBox, width, height, cx, cy, x, x1, x2, y, y1, y2, r, stroke, strokeWidth, fill, style, id, offset, stopColor, stopOpacity)
import Svg.Events exposing (on, onClick, onMouseOver)
import Task
import Time exposing (Time, second)
import Random
import Window
import Visualization.Force as Force
import Json.Decode as Decode exposing (Value, field)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { screen : Window.Size
    , particles : List Particle
    , links : List Link
    , simulation : Force.State ParticleId
    , tool : Tool
    }


type alias Position =
    { x : Int
    , y : Int
    }


type Tool
    = Add (Maybe ParticleId)
    | Delete


sameTool : Tool -> Tool -> Bool
sameTool tool1 tool2 =
    case ( tool1, tool2 ) of
        ( Add _, Add _ ) ->
            True

        ( Delete, Delete ) ->
            True

        _ ->
            False


type alias ParticleId =
    Int


type alias Link =
    ( ParticleId, ParticleId )


type alias Particle =
    Force.Entity ParticleId {}


init : ( Model, Cmd Msg )
init =
    ( { screen = { width = 0, height = 0 }
      , particles = []
      , links = []
      , simulation = Force.simulation []
      , tool = Delete
      }
    , Task.perform Init Window.size
    )



-- UPDATE


type Msg
    = Init Window.Size
    | ParticleInit (List Particle)
    | Resize Window.Size
    | Tick Time
    | ChangeTool Tool
    | ActivateParticle ParticleId
    | ActivateLink Link
    | ActivateSpace Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init ({ width, height } as screen) ->
            let
                toParticle id ( x, y ) =
                    { id = id
                    , x = (x |> toFloat)
                    , y = (y |> toFloat)
                    , vx = 0
                    , vy = 0
                    }
            in
                ( { model | screen = screen }
                , Random.pair
                    (Random.int 0 width)
                    (Random.int 0 height)
                    |> Random.list 40
                    |> Random.map (List.indexedMap toParticle)
                    |> Random.generate ParticleInit
                )

        ParticleInit particles ->
            let
                links =
                    initLinks (particles |> List.map .id)

                newModel =
                    { model
                        | particles = particles
                        , links = links
                    }
            in
                ( { newModel
                    | simulation = initSim newModel
                  }
                , Cmd.none
                )

        Resize screen ->
            ( { model | screen = screen }
            , Cmd.none
            )

        Tick _ ->
            let
                ( simulation, particles ) =
                    model.particles
                        |> Force.tick model.simulation
            in
                ( { model
                    | particles = particles
                    , simulation = simulation |> Force.reheat
                  }
                , Cmd.none
                )

        ChangeTool tool ->
            ( { model | tool = tool }
            , Cmd.none
            )

        ActivateParticle id ->
            let
                newModel =
                    case model.tool of
                        Add Nothing ->
                            { model | tool = Add (Just id) }

                        Add (Just otherId) ->
                            let
                                link =
                                    ( id, otherId )
                            in
                                { model
                                    | links = link :: (model.links |> List.filter (not << sameLink link))
                                    , tool = Add Nothing
                                }
                                    |> updateSim

                        Delete ->
                            { model
                                | particles = model.particles |> List.filter (.id >> (/=) id)
                                , links = model.links |> List.filter (not << hasParticle id)
                            }
                                |> updateSim
            in
                ( newModel
                , Cmd.none
                )

        ActivateLink link ->
            let
                newModel =
                    case model.tool of
                        Add _ ->
                            { model | tool = Add Nothing }

                        Delete ->
                            { model
                                | links = model.links |> List.filter (not << sameLink link)
                            }
                                |> updateSim
            in
                ( newModel
                , Cmd.none
                )

        ActivateSpace { x, y } ->
            let
                newParticle =
                    { id =
                        model.particles
                            |> List.Extra.maximumBy .id
                            |> Maybe.map .id
                            |> Maybe.withDefault 0
                            |> ((+) 1)
                    , x = x |> toFloat
                    , y = y |> toFloat
                    , vx = 0
                    , vy = 0
                    }

                newModel =
                    case model.tool of
                        Add Nothing ->
                            { model
                                | particles = newParticle :: model.particles
                            }
                                |> updateSim

                        Add (Just id) ->
                            { model
                                | particles = newParticle :: model.particles
                                , links = ( id, newParticle.id ) :: model.links
                                , tool = Add (Just newParticle.id)
                            }
                                |> updateSim

                        Delete ->
                            model
            in
                ( newModel
                , Cmd.none
                )


sameLink : Link -> Link -> Bool
sameLink ( from1, to1 ) ( from2, to2 ) =
    ( from1, to1 ) == ( from2, to2 ) || ( from1, to1 ) == ( to2, from2 )


hasParticle : ParticleId -> Link -> Bool
hasParticle id ( from, to ) =
    id == from || id == to


initLinks : List ParticleId -> List Link
initLinks particles =
    let
        particlesTwice =
            particles ++ particles
    in
        [ (particles
            |> List.Extra.zip (particlesTwice |> List.drop 1)
            |> List.indexedMap (,)
            |> List.map Tuple.second
          )
        , (particles
            |> List.Extra.zip (particlesTwice |> List.drop 2)
            |> List.indexedMap (,)
            |> List.filter (\( n, _ ) -> n % 2 == 1)
            |> List.map Tuple.second
          )
        , (particles
            |> List.Extra.zip (particlesTwice |> List.drop 4)
            |> List.indexedMap (,)
            |> List.filter (\( n, _ ) -> n % 4 == 2)
            |> List.map Tuple.second
          )
        , (particles
            |> List.Extra.zip (particlesTwice |> List.drop 8)
            |> List.indexedMap (,)
            |> List.filter (\( n, _ ) -> n % 8 == 3)
            |> List.map Tuple.second
          )
        ]
            |> List.concat


initSim : Model -> Force.State ParticleId
initSim model =
    Force.simulation
        [ Force.center
            ((model.screen.width |> toFloat) / 2)
            ((model.screen.height |> toFloat) / 2)
        , Force.manyBody (model.particles |> List.map .id)
        , Force.links model.links
        ]


updateSim : Model -> Model
updateSim model =
    { model | simulation = initSim model }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (second / 30) Tick
        , Window.resizes Resize
        ]



-- VIEW


position : Decode.Decoder Position
position =
    Decode.map2
        Position
        (Decode.field "clientX" Decode.int)
        (Decode.field "clientY" Decode.int)


view : Model -> Html Msg
view model =
    let
        svgViewBox =
            viewBox <| "0 0 " ++ (toString model.screen.width) ++ " " ++ (toString model.screen.height)

        svgStyle =
            style "position: fixed; top: 0; left: 0; width: 100%; height: 100%;"
    in
        svg
            [ svgViewBox
            , svgStyle
            ]
            ([ [ rect
                    [ x "0"
                    , y "0"
                    , width (toString model.screen.width)
                    , height (toString model.screen.height)
                    , fill "white"
                    , on "click" (position |> Decode.map ActivateSpace)
                    ]
                    []
               ]
             , model.tool |> toolsView
             , (model.links
                    |> List.map (linkParticlesMaybe model.particles)
                    |> Maybe.Extra.values
                    |> List.map linkView
               )
             , (model.particles |> List.map (particleView model.tool))
             ]
                |> List.concat
            )


findParticle : ParticleId -> List Particle -> Maybe Particle
findParticle id =
    List.Extra.find (.id >> (==) id)


toolsView : Tool -> List (Html Msg)
toolsView tool =
    [ toolView tool "#00FF00" "#007700" 0 (Add Nothing)
    , toolView tool "#FF0000" "#770000" 1 Delete
    ]


toolButtonWidth : Int
toolButtonWidth =
    30


toolView : Tool -> String -> String -> Int -> Tool -> Html Msg
toolView current colorOn colorOff leftOffset tool =
    let
        color =
            if tool |> sameTool current then
                colorOn
            else
                colorOff
    in
        rect
            [ x ((leftOffset * toolButtonWidth) |> toString)
            , y "0"
            , width (toolButtonWidth |> toString)
            , height (toolButtonWidth |> toString)
            , fill color
            , onClick (ChangeTool tool)
            ]
            []


linkParticlesMaybe : List Particle -> ( ParticleId, ParticleId ) -> Maybe ( Particle, Particle )
linkParticlesMaybe particles ( id1, id2 ) =
    Just (,)
        |> Maybe.Extra.andMap (particles |> findParticle id1)
        |> Maybe.Extra.andMap (particles |> findParticle id2)


particleView : Tool -> Particle -> Html Msg
particleView tool particle =
    let
        defaultColor =
            "black"

        fillColor =
            case tool of
                Add (Just id) ->
                    if id == particle.id then
                        "#00FF00"
                    else
                        defaultColor

                _ ->
                    defaultColor
    in
        circle
            [ cx (particle.x |> toString)
            , cy (particle.y |> toString)
            , r "10"
            , fill fillColor
            , onClick (ActivateParticle particle.id)
            ]
            []


linkView : ( Particle, Particle ) -> Html Msg
linkView ( from, to ) =
    line
        [ x1 <| toString <| from.x
        , y1 <| toString <| from.y
        , x2 <| toString <| to.x
        , y2 <| toString <| to.y
        , stroke "black"
        , strokeWidth "5"
        , onClick (ActivateLink ( from.id, to.id ))
        ]
        []
