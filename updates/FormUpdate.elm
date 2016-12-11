module FormUpdate exposing (..)

import Messages exposing (..)
import Models exposing (..)

import FormModel exposing (..)

update : FormMsg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddInput ->
      ({ model | inputs = model.inputs ++ [{ textInput | id = maxInputId model + 1 }] }, Cmd.none)
    RemoveInput id ->
      ({ model | inputs = List.filter (\input -> input.id /= id) model.inputs }, Cmd.none)
