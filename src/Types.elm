module Types (..) where

import Dict exposing (..)


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


type alias Player =
  { position : Position }


type alias Model =
  { world : World Cell
  , player : Player
  , dialogue : Maybe String
  }


canStandOn : Maybe Cell -> Bool
canStandOn cell =
  case Debug.log "Can stand on" cell of
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
