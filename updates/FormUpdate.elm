module FormUpdate exposing (..)

import Messages exposing (..)
import Models exposing (..)

import Form exposing (..)
import Inputs exposing (..)

update : FormMsg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddInput ->
      ({ model | inputs = model.inputs ++ [[{ textInput | id = maxInputId model + 1 }]] }, Cmd.none)
    RemoveInput id ->
      let
        newInputs = List.filter (\row -> List.all (\input -> input.id /= id) row ) model.inputs
      in
        ({ model | inputs = newInputs }, Cmd.none)
