module FormUpdate exposing (..)

import Messages exposing (..)
import Models exposing (..)

update msg model =
  case msg of
    AddTextInput ->
      addInputToForm model (newTextInput model)
    AddTextarea ->
      addInputToForm model (newTextArea model)
    AddSelect ->
      addInputToForm model (newSelect model)
    AddMultiselect ->
      addInputToForm model (newMultiselect model)
    AddFileUpload ->
      addInputToForm model (newFileUpload model)
    AddRadio ->
      addInputToForm model (newRadio model)
    AddCheckbox ->
      addInputToForm model (newCheckbox model)
    AddButton ->
      addInputToForm model (newButton model)
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
  TextInput { id = countNewInputId model, classList = [ "form-control" ], placeholder = Nothing, label = Just "New input", disabled = False, readonly = False, size = Normal, addon1 = Nothing, addon2 = Nothing, small = Nothing, type' = Text }

newTextArea : Model -> Input
newTextArea model =
  TextArea { id = countNewInputId model, classList = [ "form-control" ], placeholder = Nothing, label = Just "New input", rowNumber = 3, disabled = False }

newSelect : Model -> Input
newSelect model =
  Select { id = countNewInputId model, classList = [ "form-control" ], label = Just "New select", small = Nothing, disabled = False, size = Normal, options = ["options1", "option2", "option3"] }

newMultiselect : Model -> Input
newMultiselect model =
  Multiselect { id = countNewInputId model, classList = [ "form-control" ], label = Just "New input", disabled = False, options = ["option1", "option2", "option3"], small = Nothing }

newFileUpload : Model -> Input
newFileUpload model =
  FileUpload { id = countNewInputId model, classList = [ "form-control-file" ], label = Just "New input", disabled = False, small = Nothing }

newRadio : Model -> Input
newRadio model =
  Radio { id = countNewInputId model, classList = [ "form-control" ], label = Just "New input", options = ["option1", "option2"] }

newCheckbox : Model -> Input
newCheckbox model =
  Checkbox { id = countNewInputId model, classList = [ "form-control" ], label = Just "New input" }

newButton : Model -> Input
newButton model =
  Button { id = countNewInputId model, classList = [ "form-control" ], label = Just "New input" }

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
