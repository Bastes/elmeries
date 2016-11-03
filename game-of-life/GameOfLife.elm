module GameOfLife exposing (Cell(..), World, step, fate, neighboursCount, setCell, toggle, toggleCell)

import List exposing (map, sum, length, filter, drop, take, indexedMap)
import Array exposing (fromList, get)
import Maybe exposing (andThen, withDefault)

type Cell = Live | Dead
type alias World = List (List Cell)

step : World -> World
step w = indexedMap (\y -> indexedMap (\x -> fate (neighboursCount w y x))) w

neighboursCount : World -> Int -> Int -> Int
neighboursCount w y x =
  let
    neighbourhood = neighbourhoodOf y x w
    liveCount     = countLives neighbourhood
    cell          = cellAt y x w
  in
    case cell of
      Live -> liveCount - 1
      Dead -> liveCount

fate : Int -> Cell -> Cell
fate n c =
  case n of
    2 -> c
    3 -> Live
    _ -> Dead

toggle : Int -> Int -> World -> World
toggle y x = indexedMap (\cy -> indexedMap (\cx c -> if (cy /= y || cx /= x) then c else toggleCell c))

setCell : Int -> Int -> Cell -> World -> World
setCell y x c = indexedMap (\cy -> indexedMap (\cx cc -> if (cy /= y || cx /= x) then cc else c))

toggleCell : Cell -> Cell
toggleCell c = case c of
  Dead -> Live
  Live -> Dead

countLives : World -> Int
countLives = map (filter ((==) Live) >> length) >> sum

neighbourhoodOf : Int -> Int -> List (List a) -> List (List a)
neighbourhoodOf y x = take3Around y >> map (take3Around x)

take3Around : Int -> List a -> List a
take3Around i = drop (i - 1) >> take (min 3 (2 + i))

at : Int -> List a -> Maybe a
at i = get i << fromList

cellAt : Int -> Int -> World -> Cell
cellAt y x = at y >> (flip andThen) (at x) >> withDefault Dead
