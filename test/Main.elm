module Main (..) where

import Console
import ElmTest exposing (..)
import Task exposing (Task)


tests : Test
tests =
  suite
    "All"
    []


port runner : Signal (Task x ())
port runner =
  Console.run (consoleRunner tests)
