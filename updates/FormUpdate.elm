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

addNewInput : Input -> Model -> (Model, Cmd Msg)
addNewInput input model =
  let
    newInput = { input | id = maxInputId model + 1 }
  in
    ({ model | inputs = model.inputs ++ [newInput] }, Cmd.none)

removeInput : Model -> Int -> (Model, Cmd Msg)
removeInput model id =
  let
    filteredForm = List.filter (\input -> input.id /= id) model.inputs
  in
    ({ model | inputs = filteredForm }, Cmd.none)
