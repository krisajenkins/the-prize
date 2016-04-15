module State (..) where

import Types exposing (..)
import Effects exposing (..)
import Types exposing (..)
import Dict


wall : List Position -> List ( Position, Cell )
wall =
  List.map (\coords -> ( coords, Block ))


horizontalWall : Int -> List Int -> List ( Position, Cell )
horizontalWall x ys =
  ys
    |> List.map (\y -> ( x, y ))
    |> wall


verticalWall : List Int -> Int -> List ( Position, Cell )
verticalWall xs y =
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
    ((List.concatMap
        (\x ->
          List.map
            (\y -> ( ( x, y ), Path ))
            yRange
        )
        xRange
     )
      ++ (horizontalWall minX yRange
            ++ horizontalWall maxX yRange
            ++ verticalWall xRange minY
            ++ verticalWall xRange maxY
         )
    )
      |> Dict.fromList


initialModel : Model
initialModel =
  { world = initialWorld
  , dialogue = Just "Well, I guess I'd better look around..."
  , player = { position = ( 3, 3 ) }
  }


initialEffects : Effects Action
initialEffects =
  none


updateWithDialogue : Action -> Model -> ( Model, Maybe String, Effects Action )
updateWithDialogue action model =
  case Debug.log "Action" action of
    MoveTo newPosition ->
      if canStandOn (objectAt model.world newPosition) then
        let
          p =
            model.player

          newPlayer =
            { p | position = newPosition }
        in
          ( { model | player = newPlayer }
          , Nothing
          , none
          )
      else
        ( model
        , Just "I can't do that Dave."
        , none
        )


update : Action -> Model -> ( Model, Effects Action )
update action model =
  let
    ( newModel, dialogue, newEffects ) =
      updateWithDialogue action model
  in
    ( { newModel | dialogue = dialogue }
    , newEffects
    )
