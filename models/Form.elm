module Form exposing (..)

import Inputs exposing (..)

-- type GroupOfInputs = List Inputs.Input
-- type Row = SingleInput Inputs.Input | GroupOfInputs (List Inputs.Input)
type alias Row = List Inputs.Input

-- type alias Form = List Inputs.Input
type alias Form = List Row

mapInputs func rows =
  List.map (\row -> List.map (\input -> func input) row) rows
