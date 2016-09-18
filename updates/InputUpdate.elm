module InputUpdate exposing (..)

import Messages exposing (..)
import Models exposing (..)

update msg model =
  case msg of
    RemoveInput id ->
      removeInput model id
    EditInput id ->
      (model, Cmd.none)

-------------
-- Private --
-------------

removeInput model inputId =
  let
    newElement = removeElementsRecursive model inputId
  in
    ({ model | element = newElement }, Cmd.none)
