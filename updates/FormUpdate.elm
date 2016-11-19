module FormUpdate exposing (..)

import Array exposing (..)
import Messages exposing (..)
import Models exposing (..)

import FormModel exposing (..)

update msg model =
  case msg of
    AddInput ->
      addNewInput textInput model
    RemoveInput id ->
      removeInput model id
    EditInput id ->
      ({ model | currentlyEdditedInputId = Just id }, Cmd.none)
    StopEditing ->
      ({ model | currentlyEdditedInputId = Nothing }, Cmd.none)

addNewInput : Input -> Model -> (Model, Cmd Msg)
addNewInput input model =
  let
    newInput = { input | id = maxInputId model + 1 }
  in
    ({ model | form = model.form ++ [newInput] }, Cmd.none)

removeInput : Model -> Id -> (Model, Cmd Msg)
removeInput model id =
  let
    filteredForm = List.filter (\input -> input.id /= id) model.form
  in
    ({ model | form = filteredForm }, Cmd.none)
