module Main exposing (..)

import List.Extra
import Maybe.Extra
import Html exposing (Html)
import Svg exposing (svg, circle, node, line, radialGradient, stop, defs)
import Svg.Attributes exposing (viewBox, width, height, cx, cy, x1, x2, y1, y2, r, stroke, strokeWidth, fill, style, id, offset, stopColor, stopOpacity)
import Svg.Events exposing (onClick, onMouseOver)
import Task
import Time exposing (Time, second)
import Random
import Window
import Visualization.Force as Force


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
    , simulation : Force.State Int
    , hovering : Maybe Hovering
    , selected : Maybe Selected
    }


type Hovering
    = HoveringLink ( Int, Int )
    | HoveringParticle Int


type alias Selected =
    Int


type alias Link =
    ( Int, Int )


type alias Particle =
    Force.Entity Int {}


init : ( Model, Cmd Msg )
init =
    ( { screen = { width = 0, height = 0 }
      , particles = []
      , links = []
      , simulation = Force.simulation []
      , hovering = Nothing
      , selected = Nothing
      }
    , Task.perform Init Window.size
    )



-- UPDATE


type Msg
    = Init Window.Size
    | ParticleInit (List Particle)
    | Resize Window.Size
    | Tick Time
    | Activate
    | Hover Hovering


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

        Activate ->
            let
                newModel =
                    case ( model.hovering, model.selected ) of
                        ( Just (HoveringLink link), _ ) ->
                            { model | links = model.links |> List.filter ((/=) link) }

                        ( Just (HoveringParticle id), Just selectedId ) ->
                            if id == selectedId then
                                { model
                                    | particles = model.particles |> List.filter (.id >> (/=) id)
                                    , links = model.links |> List.filter (\( id1, id2 ) -> id1 /= id && id2 /= id)
                                    , selected = Nothing
                                }
                            else
                                { model
                                    | links = ( id, selectedId ) :: model.links
                                    , selected = Nothing
                                }

                        ( Just (HoveringParticle id), Nothing ) ->
                            { model | selected = Just id }

                        _ ->
                            model
            in
                ( { newModel
                    | simulation = initSim newModel
                  }
                , Cmd.none
                )

        Hover hovering ->
            ( { model | hovering = Just hovering }
            , Cmd.none
            )


initLinks : List Int -> List Link
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


initSim : Model -> Force.State Int
initSim model =
    Force.simulation
        [ Force.center
            ((model.screen.width |> toFloat) / 2)
            ((model.screen.height |> toFloat) / 2)
        , Force.manyBody (model.particles |> List.map .id)
        , Force.links model.links
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (second / 30) Tick
        , Window.resizes Resize
        ]



-- VIEW


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
            , onClick Activate
            ]
            ([ (model.links
                    |> List.map (linkParticlesMaybe model.particles)
                    |> Maybe.Extra.values
                    |> List.map (linkView model.hovering)
               )
             , (model.particles |> List.map (particleView model.hovering model.selected))
             ]
                |> List.concat
            )


findParticle : Int -> List Particle -> Maybe Particle
findParticle id =
    List.Extra.find (.id >> (==) id)


linkParticlesMaybe : List Particle -> ( Int, Int ) -> Maybe ( Particle, Particle )
linkParticlesMaybe particles ( id1, id2 ) =
    Just (,)
        |> Maybe.Extra.andMap (particles |> findParticle id1)
        |> Maybe.Extra.andMap (particles |> findParticle id2)


particleView : Maybe Hovering -> Maybe Selected -> Particle -> Html Msg
particleView hovering selected particle =
    let
        fillColor =
            (if selected == Just particle.id then
                "lightgreen"
             else if hovering == Just (HoveringParticle particle.id) then
                "red"
             else
                "black"
            )
    in
        circle
            [ cx (particle.x |> toString)
            , cy (particle.y |> toString)
            , r "10"
            , fill fillColor
            , onMouseOver (Hover (HoveringParticle particle.id))
            ]
            []


linkView : Maybe Hovering -> ( Particle, Particle ) -> Html Msg
linkView hovering ( from, to ) =
    let
        strokeColor =
            (if hovering == Just (HoveringLink ( from.id, to.id )) then
                "red"
             else
                "black"
            )
    in
        line
            [ x1 <| toString <| from.x
            , y1 <| toString <| from.y
            , x2 <| toString <| to.x
            , y2 <| toString <| to.y
            , stroke strokeColor
            , strokeWidth "5"
            , onMouseOver (Hover (HoveringLink ( from.id, to.id )))
            ]
            []
