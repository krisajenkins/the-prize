module View (root) where

import Signal exposing (..)
import Html exposing (Html)
import Types exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Dict


root : Address Action -> Model -> Html
root address model =
  svg
    [ width "1000px"
    , height "1000px"
    ]
    [ g
        []
        (model.world
          |> Dict.toList
          |> List.map (uncurry (tile address))
        )
    , tile address model.player.position Character
    , text'
        [ x "0px"
        , y "340px"
        ]
        [ text (Maybe.withDefault "" model.dialogue) ]
    ]


tileSize : Int
tileSize =
  40


tile : Address Action -> Position -> Cell -> Svg
tile address position cell =
  let
    colours =
      case cell of
        Path ->
          [ stroke "grey", fill "white" ]

        Block ->
          [ stroke "black", fill "chartreuse" ]

        Character ->
          [ stroke "grey", fill "turquoise" ]
  in
    rect
      ([ x (toString (fst position * tileSize))
       , y (toString (snd position * tileSize))
       , width (toString tileSize ++ "px")
       , height (toString tileSize ++ "px")
       , onClick (Signal.message address (MoveTo position))
       ]
        ++ colours
      )
      []
