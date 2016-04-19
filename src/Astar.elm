module Astar (findPath, Position, Path) where

import Set exposing (Set)
import Dict exposing (Dict)
import Array exposing (Array)


type alias Position =
  ( Int, Int )


type alias Path =
  Array Position


type alias Model =
  { evaluated : Set Position
  , openSet : Set Position
  , costs : Dict Position Int
  , cameFrom : Dict Position Position
  }


initialModel : Position -> Model
initialModel start =
  { evaluated = Set.empty
  , openSet = Set.singleton start
  , costs = Dict.singleton start 0
  , cameFrom = Dict.empty
  }


cheapestOpen : (Position -> Int) -> Model -> Maybe Position
cheapestOpen costFn model =
  model.openSet
    |> Set.toList
    |> List.filterMap
        (\position ->
          case Dict.get position model.costs of
            Nothing ->
              Nothing

            Just cost ->
              Just ( position, cost + costFn position )
        )
    |> List.sortBy snd
    |> List.head
    |> Maybe.map fst


reconstructPath : Dict Position Position -> Position -> Path
reconstructPath cameFrom goal =
  case Dict.get goal cameFrom of
    Nothing ->
      Array.empty

    Just next ->
      Array.push
        goal
        (reconstructPath cameFrom next)


bestCost : Int -> Maybe Int -> Int
bestCost newDistance oldDistance =
  case oldDistance of
    Nothing ->
      newDistance

    Just distance ->
      min distance newDistance


updateCost : Position -> Position -> Model -> Model
updateCost next neighbour model =
  let
    newCameFrom =
      Dict.insert neighbour next model.cameFrom

    distanceTo =
      Array.length (reconstructPath newCameFrom neighbour)

    newModel =
      { model
        | costs = Dict.insert neighbour distanceTo model.costs
        , cameFrom = newCameFrom
      }
  in
    case Dict.get neighbour model.costs of
      Nothing ->
        newModel

      Just previousDistance ->
        if distanceTo < previousDistance then
          newModel
        else
          model


astar : (Position -> Position -> Int) -> (Position -> Set Position) -> Position -> Model -> Maybe Path
astar costFn movesFrom goal model =
  case cheapestOpen (costFn goal) model of
    Nothing ->
      Nothing

    Just next ->
      if next == goal then
        Just (reconstructPath model.cameFrom goal)
      else
        let
          modelPopped =
            { model
              | openSet = Set.remove next model.openSet
              , evaluated = Set.insert next model.evaluated
            }

          neighbours =
            movesFrom next

          newNeighbours =
            Set.diff neighbours modelPopped.evaluated

          modelWithNeighbours =
            { modelPopped
              | openSet = Set.union modelPopped.openSet newNeighbours
            }

          modelWithCosts =
            Set.foldl
              (updateCost next)
              modelWithNeighbours
              newNeighbours
        in
          astar costFn movesFrom goal modelWithCosts


findPath : (Position -> Position -> Int) -> (Position -> Set Position) -> Position -> Position -> Maybe Path
findPath costFn movesFrom start goal =
  initialModel start
    |> astar costFn movesFrom goal
