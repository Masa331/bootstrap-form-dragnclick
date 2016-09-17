module Updates exposing (..)

import Messages exposing (..)
import Models exposing (..)

update msg model =
  case msg of
    AddTextInput ->
      addInputToForm model textInput

-------------
-- Private --
-------------

addInputToForm model input =
  (model, Cmd.none)
