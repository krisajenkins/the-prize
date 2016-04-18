module Types (..) where

import Dict exposing (Dict)
import Set exposing (Set)


type alias Position =
  ( Int, Int )


type alias World a =
  Dict Position a


type Cell
  = Block
  | Path
  | Character


type Action
  = MoveTo Position
  | Tick


type alias Player =
  { position : Position }


type alias Model =
  { world : World Cell
  , player : Player
  , dialogue : Maybe String
  , destination : Maybe Position
  }


canStandOn : Maybe Cell -> Bool
canStandOn cell =
  case cell of
    Nothing ->
      True

    Just Path ->
      True

    Just Block ->
      False

    Just Character ->
      False


objectAt : World Cell -> Position -> Maybe Cell
objectAt =
  flip Dict.get


estimatedDistance : Position -> Position -> Int
estimatedDistance ( x1, y1 ) ( x2, y2 ) =
  (abs (x1 - x2))
    + (abs (y1 - y2))


movesFrom : Position -> Set Position
movesFrom ( x, y ) =
  Set.fromList
    [ ( x - 1, y )
    , ( x + 1, y )
    , ( x, y - 1 )
    , ( x, y + 1 )
    ]


validMovesFrom : World Cell -> Position -> Set Position
validMovesFrom world position =
  Set.filter
    (canStandOn << objectAt world)
    (movesFrom position)
