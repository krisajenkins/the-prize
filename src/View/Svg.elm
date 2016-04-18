module View.Svg (root) where

import Common.View exposing (..)
import Signal exposing (..)
import Html exposing (Html)
import Types exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Dict


tileSize : Int
tileSize =
  40


root : Address Action -> Model -> Html
root address model =
  svg
    [ width (px 800)
    , height (px 400)
    ]
    [ g
        []
        (model.world
          |> Dict.toList
          |> List.map (uncurry (tile address))
        )
    , tile address model.player.position Character
    ]


tile : Address Action -> Position -> Cell -> Svg
tile address position cell =
  let
    colours =
      case cell of
        Path ->
          [ stroke "grey", fill "#fdfdfd" ]

        Block ->
          [ stroke "#0466da", fill "#04c9da" ]

        Character ->
          [ stroke "#8504da", fill "#e4049a" ]
  in
    rect
      ([ x (toString (fst position * tileSize))
       , y (toString (snd position * tileSize))
       , width (px tileSize)
       , height (px tileSize)
       , onClick (Signal.message address (MoveTo position))
       ]
        ++ colours
      )
      []
