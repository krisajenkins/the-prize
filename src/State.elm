module State (..) where

import Types exposing (..)
import Effects exposing (..)
import Dict
import Time exposing (Time)
import Task exposing (Task)
import Astar
import Array


wall : List Position -> List ( Position, Cell )
wall =
  List.map (\coords -> ( coords, Block ))


verticalWall : Int -> List Int -> List ( Position, Cell )
verticalWall x ys =
  ys
    |> List.map (\y -> ( x, y ))
    |> wall


horizontalWall : List Int -> Int -> List ( Position, Cell )
horizontalWall xs y =
  xs
    |> List.map (\x -> ( x, y ))
    |> wall


initialWorld : World Cell
initialWorld =
  let
    ( minX, minY ) =
      ( 0, 0 )

    ( maxX, maxY ) =
      ( 15, 7 )

    xRange =
      [minX..maxX]

    yRange =
      [minY..maxY]
  in
    (List.concat
      [ List.concatMap
          (\x ->
            List.map
              (\y -> ( ( x, y ), Path ))
              yRange
          )
          xRange
      , verticalWall minX yRange
      , verticalWall maxX yRange
      , horizontalWall xRange minY
      , horizontalWall xRange maxY
      , horizontalWall [4..10] 4
      , verticalWall 4 [4..5]
      , verticalWall 8 [3..5]
      , horizontalWall [12..13] 2
      ]
    )
      |> Dict.fromList


initialModel : Model
initialModel =
  { world = initialWorld
  , dialogue = Just "Well, I guess I'd better look around..."
  , player = { position = ( 3, 3 ) }
  , destination = Nothing
  }


initialEffects : Effects Action
initialEffects =
  none


updateWithDialogue : Action -> Model -> ( Model, Maybe String )
updateWithDialogue action model =
  case action of
    MoveTo newDestination ->
      if canStandOn (objectAt model.world newDestination) then
        ( { model | destination = Just newDestination }
        , Nothing
        )
      else
        ( model
        , Just "I can't do that Dave."
        )

    Tick ->
      case model.destination of
        Nothing ->
          ( model, Nothing )

        Just destination ->
          case
            Astar.findPath
              estimatedDistance
              (validMovesFrom model.world)
              (model.player.position)
              destination
          of
            Nothing ->
              ( { model | destination = Nothing }
              , Just "Doesn't look like I'll make it."
              )

            Just path ->
              case Array.get 0 path of
                Nothing ->
                  ( { model | destination = Nothing }
                  , Just "J'arrive!"
                  )

                Just p ->
                  let
                    player =
                      model.player

                    newPlayer =
                      { player | position = p }
                  in
                    ( { model | player = newPlayer }
                    , Just ("Step.  " ++ (toString (Array.length path - 1)) ++ " more to go.")
                    )


update : Action -> Model -> Model
update action model =
  let
    ( newModel, dialogue ) =
      updateWithDialogue action model
  in
    { newModel | dialogue = dialogue }


effects : Action -> ( Model, Model ) -> Effects Action
effects action ( _, model ) =
  case model.destination of
    Nothing ->
      none

    Just _ ->
      tickIn (200 * Time.millisecond)


tickIn : Time -> Effects Action
tickIn t =
  Task.sleep t
    |> Effects.task
    |> Effects.map (always Tick)
