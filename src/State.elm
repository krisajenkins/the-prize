module State (..) where

import Effects exposing (..)
import Types exposing (..)


initialModel : Model
initialModel =
  {}


initialEffects : Effects Action
initialEffects =
  none


update : Action -> Model -> ( Model, Effects Action )
update action model =
  ( model, none )
