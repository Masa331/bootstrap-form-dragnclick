module FormUpdate exposing (..)

import Messages exposing (..)
import Models exposing (..)

import FormModel exposing (..)

update msg model =
  case msg of
    AddTextInput ->
      addNewInput textInput model
    AddTextarea ->
      addNewInput textArea model
    AddSelect ->
      addNewInput select model
    AddMultiselect ->
      addNewInput multiselect model
    AddFileUpload ->
      addNewInput fileUpload model
    AddRadio ->
      addNewInput radio model
    AddCheckbox ->
      addNewInput checkbox model
    AddButton ->
      addNewInput button model
    RemoveInput id ->
      removeInput model id
    EditInput id ->
      ({ model | currentlyEdditedInputId = Just id }, Cmd.none)
    StopEditing ->
      ({ model | currentlyEdditedInputId = Nothing }, Cmd.none)
    MoveUp id ->
      (model, Cmd.none)
    MoveDown id ->
      (model, Cmd.none)

moveInputUp : Id -> Model -> (Model, Cmd Msg)
moveInputUp id model =
  Models.moveInputUp id model
  -- (model, Cmd.none)


addNewInput : (Int -> Input) -> Model -> (Model, Cmd Msg)
addNewInput input model =
  let
    newId = maxInputId model + 1
  in
    ({ model | form = model.form ++ [input newId] }, Cmd.none)

removeInput : Model -> Id -> (Model, Cmd Msg)
removeInput model id =
  let
    filteredForm = List.filter (\input -> input.id /= id) model.form
  in
    ({ model | form = filteredForm }, Cmd.none)
