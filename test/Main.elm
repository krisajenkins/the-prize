module Main (..) where

import Console
import Task exposing (Task)
import ElmTest exposing (..)


tests : Test
tests =
  suite
    "All"
    []


port runner : Signal (Task x ())
port runner =
  Console.run (consoleRunner tests)
