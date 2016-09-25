module FormUpdate exposing (..)

import Messages exposing (..)
import Models exposing (..)

update msg model =
  case msg of
    AddTextInput ->
      addInputToForm model (newTextInput model)
    AddSelect ->
      addInputToForm model (newTextInput model)
    AddMultiselect ->
      addInputToForm model (newTextInput model)
    AddTextarea ->
      addInputToForm model (newTextInput model)
    AddFileUpload ->
      addInputToForm model (newTextInput model)
    AddRadioButtons ->
      addInputToForm model (newTextInput model)
    AddCheckbox ->
      addInputToForm model (newTextInput model)
    AddButton ->
      addInputToForm model (newTextInput model)
    RemoveInput id ->
      removeInput model id
    EditInput id ->
      ({ model | currentlyEdditedInputId = Just id }, Cmd.none)
    StopEditing ->
      ({ model | currentlyEdditedInputId = Nothing }, Cmd.none)

removeInput model id =
  let
    filteredForm = List.filter (\input -> (extractId input) /= id) model.form
  in
    ({ model | form = filteredForm }, Cmd.none)

newTextInput : Model -> Input
newTextInput model =
  TextInput { id = countNewInputId model, classList = [ "form-control" ], placeholder = Nothing, label = Just "New input" }

-------------
-- Private --
-------------

countNewInputId : Model -> Id
countNewInputId model =
  let
    actuall = extractMaxId model.form
  in
    actuall + 1


addInputToForm model inp =
  let
    newForm = List.append model.form [inp]
  in
    ({ model | form = newForm }, Cmd.none)

extractMaxId : List Input -> Int
extractMaxId inputs =
  let
    allIds = List.map extractId inputs
  in
    Maybe.withDefault 1 (List.maximum allIds)
