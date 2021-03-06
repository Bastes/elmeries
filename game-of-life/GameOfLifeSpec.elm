module Main exposing (..)

import Test exposing (..)
import Expect
import Test.Runner.Html
import Fuzz exposing (intRange)
import GameOfLife exposing (..)


main =
    [ describe "the GameOfLife module"
        [ describe "step"
            [ test "the next step with a single dead cell is staying dead" <|
                \() ->
                    step [ [ Dead ] ] |> Expect.equal [ [ Dead ] ]
            , test "the next step with a single live cell is becoming dead" <|
                \() ->
                    step [ [ Live ] ] |> Expect.equal [ [ Dead ] ]
            , test "the next step with a single dead cell surrounded by two live ones is to stay alive" <|
                \() ->
                    let
                        worldBefore =
                            [ [ Live, Dead, Dead ]
                            , [ Dead, Live, Dead ]
                            , [ Dead, Dead, Live ]
                            ]

                        worldAfter =
                            [ [ Dead, Dead, Dead ]
                            , [ Dead, Live, Dead ]
                            , [ Dead, Dead, Dead ]
                            ]
                    in
                        step worldBefore |> Expect.equal worldAfter
            , test "the next step of an alternator is the alternative" <|
                \() ->
                    let
                        worldBefore =
                            [ [ Dead, Live, Dead ]
                            , [ Dead, Live, Dead ]
                            , [ Dead, Live, Dead ]
                            ]

                        worldAfter =
                            [ [ Dead, Dead, Dead ]
                            , [ Live, Live, Live ]
                            , [ Dead, Dead, Dead ]
                            ]
                    in
                        step worldBefore |> Expect.equal worldAfter
            ]
        , describe "neighboursCount"
            [ test "the central cell has three neighbours" <|
                \() ->
                    let
                        world =
                            [ [ Live, Dead, Dead ]
                            , [ Dead, Dead, Live ]
                            , [ Dead, Live, Dead ]
                            ]
                    in
                        neighboursCount world 1 1 |> Expect.equal 3
            , test "the central cell has two neighbours" <|
                \() ->
                    let
                        world =
                            [ [ Dead, Dead, Dead ]
                            , [ Dead, Dead, Live ]
                            , [ Dead, Live, Dead ]
                            ]
                    in
                        neighboursCount world 1 1 |> Expect.equal 2
            , test "the central cell is live and has two neighbours" <|
                \() ->
                    let
                        world =
                            [ [ Dead, Dead, Dead ]
                            , [ Dead, Live, Live ]
                            , [ Dead, Live, Dead ]
                            ]
                    in
                        neighboursCount world 1 1 |> Expect.equal 2
            , test "the 1-1 cell is live and has two neighbours in a bigger world" <|
                \() ->
                    let
                        world =
                            [ [ Dead, Dead, Dead, Dead ]
                            , [ Dead, Live, Live, Live ]
                            , [ Dead, Live, Dead, Live ]
                            , [ Dead, Live, Dead, Live ]
                            ]
                    in
                        neighboursCount world 1 1 |> Expect.equal 2
            , test "the top left cell is dead and has one neighbour" <|
                \() ->
                    let
                        world =
                            [ [ Dead, Dead, Dead ]
                            , [ Dead, Live, Live ]
                            , [ Dead, Live, Dead ]
                            ]
                    in
                        neighboursCount world 0 0 |> Expect.equal 1
            , test "the bottom right cell is dead and has two neighbour" <|
                \() ->
                    let
                        world =
                            [ [ Dead, Dead, Dead ]
                            , [ Dead, Live, Live ]
                            , [ Dead, Dead, Dead ]
                            ]
                    in
                        neighboursCount world 2 2 |> Expect.equal 2
            ]
        , describe "toggleCell"
            [ test "a dead cell is called back to life" <|
                \() -> toggleCell Dead |> Expect.equal Live
            , test "a live cell is killed" <|
                \() -> toggleCell Live |> Expect.equal Dead
            ]
        , describe "fate"
            [ fuzz (intRange 0 1) "the fate of a live cell with zero to one neighbour is to die" <|
                \neighbours ->
                    fate neighbours Live |> Expect.equal Dead
            , fuzz (intRange 2 3) "the fate of a live cell with two or three neighbour is to stay aliiiiive" <|
                \neighbours ->
                    fate neighbours Live |> Expect.equal Live
            , fuzz (intRange 4 8) "the fate of a live cell with 4 to 8 neighbours is to die, overcrowded" <|
                \neighbours ->
                    fate neighbours Live |> Expect.equal Dead
            , fuzz (intRange 0 2) "the fate of a dead cell with 0 to 2 neighbours is to stay dead" <|
                \neighbours ->
                    fate neighbours Dead |> Expect.equal Dead
            , test "the fate of a dead cell with 3 neighbours is to be born again" <|
                \() ->
                    fate 3 Dead |> Expect.equal Live
            , fuzz (intRange 4 8) "the fate of a dead cell with 4 to 8 neighbours is to stay dead still" <|
                \neighbours ->
                    fate neighbours Dead |> Expect.equal Dead
            ]
        , describe "setCell"
            [ test "sets the one cell to Alive" <|
                \() ->
                    let
                        worldBefore =
                            [ [ Live, Dead, Dead ]
                            , [ Dead, Live, Dead ]
                            , [ Dead, Dead, Live ]
                            ]

                        worldAfter =
                            [ [ Live, Dead, Dead ]
                            , [ Dead, Live, Live ]
                            , [ Dead, Dead, Live ]
                            ]
                    in
                        setCell 1 2 Live worldBefore |> Expect.equal worldAfter
            , test "keeps the one cell to Alive" <|
                \() ->
                    let
                        world =
                            [ [ Live, Dead, Dead ]
                            , [ Dead, Live, Live ]
                            , [ Dead, Dead, Live ]
                            ]
                    in
                        setCell 0 0 Live world |> Expect.equal world
            , test "sets the one cell to Dead" <|
                \() ->
                    let
                        worldBefore =
                            [ [ Live, Dead, Dead ]
                            , [ Dead, Live, Dead ]
                            , [ Dead, Dead, Live ]
                            ]

                        worldAfter =
                            [ [ Live, Dead, Dead ]
                            , [ Dead, Dead, Dead ]
                            , [ Dead, Dead, Live ]
                            ]
                    in
                        setCell 1 1 Dead worldBefore |> Expect.equal worldAfter
            , test "keeps the one cell to Dead" <|
                \() ->
                    let
                        world =
                            [ [ Live, Dead, Dead ]
                            , [ Dead, Live, Dead ]
                            , [ Dead, Dead, Live ]
                            ]
                    in
                        setCell 0 2 Dead world |> Expect.equal world
            ]
        , describe "toggle"
            [ test "toggles the one cell from Dead to Alive" <|
                \() ->
                    let
                        worldBefore =
                            [ [ Live, Dead, Dead ]
                            , [ Dead, Live, Dead ]
                            , [ Dead, Dead, Live ]
                            ]

                        worldAfter =
                            [ [ Live, Live, Dead ]
                            , [ Dead, Live, Dead ]
                            , [ Dead, Dead, Live ]
                            ]
                    in
                        toggle 0 1 worldBefore |> Expect.equal worldAfter
            , test "toggles the one cell from Alive to Dead" <|
                \() ->
                    let
                        worldBefore =
                            [ [ Live, Dead, Dead ]
                            , [ Dead, Live, Dead ]
                            , [ Dead, Dead, Live ]
                            ]

                        worldAfter =
                            [ [ Live, Dead, Dead ]
                            , [ Dead, Live, Dead ]
                            , [ Dead, Dead, Dead ]
                            ]
                    in
                        toggle 2 2 worldBefore |> Expect.equal worldAfter
            ]
        ]
    ]
        |> concat
        |> Test.Runner.Html.run
