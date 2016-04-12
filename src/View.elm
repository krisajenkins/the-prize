module View (root) where

import Signal exposing (..)
import Html exposing (..)
import Types exposing (..)


root : Address Action -> Model -> Html
root address model =
  text "Hello World!"
