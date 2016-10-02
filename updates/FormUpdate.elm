module FormUpdate exposing (..)

import Messages exposing (..)
import Models exposing (..)

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

addNewInput input model =
  let
    newId = countNewInputId model
  in
    ({ model | form = model.form ++ [input newId] }, Cmd.none)

countNewInputId : Model -> Id
countNewInputId model =
  maxId model + 1

maxId : Model -> Int
maxId model =
  Maybe.withDefault 0
  <| List.maximum
  <| List.map extractId model.form

removeInput model id =
  let
    filteredForm = List.filter (\input -> (extractId input) /= id) model.form
  in
    ({ model | form = filteredForm }, Cmd.none)

textInput : Int -> Input
textInput id =
  TextInput { id = id, classList = [ "form-control" ], placeholder = Nothing, label = Just "New input", disabled = False, readonly = False, size = Normal, addon1 = Nothing, addon2 = Nothing, small = Nothing, type' = Text }

textArea : Int -> Input
textArea id =
  TextArea { id = id, classList = [ "form-control" ], placeholder = Nothing, label = Just "New input", rowNumber = 3, disabled = False }

select : Int -> Input
select id =
  Select { id = id, classList = [ "form-control" ], label = Just "New select", small = Nothing, disabled = False, size = Normal, options = ["options1", "option2", "option3"] }

multiselect : Int -> Input
multiselect id =
  Multiselect { id = id, classList = [ "form-control" ], label = Just "New input", disabled = False, options = ["option1", "option2", "option3"], small = Nothing }

fileUpload : Int -> Input
fileUpload id =
  FileUpload { id = id, classList = [ "form-control-file" ], label = Just "New input", disabled = False, small = Nothing }

radio : Int -> Input
radio id =
  Radio { id = id, classList = [ "form-control" ], label = Just "New input", options = ["option1", "option2"] }

checkbox : Int -> Input
checkbox id =
  Checkbox { id = id, classList = [ "form-control" ], label = Just "New input" }

button : Int -> Input
button id =
  Button { id = id, classList = [ "form-control" ], label = Just "New input" }

-------------
-- Private --
-------------
