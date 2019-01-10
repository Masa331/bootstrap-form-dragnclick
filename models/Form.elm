module Form exposing (..)

import Inputs exposing (..)

type alias Row = List Inputs.Input

type alias Form = List Row

mapInputs func rows =
  List.map (\row -> List.map (\input -> func input) row) rows
