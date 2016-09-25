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

newTextInput : Model -> Input
newTextInput model =
  TextInput (countNewInputId model, [ "form-control" ], Nothing, Just "Some input")

-- -------------
-- -- Private --
-- -------------

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
    max = Maybe.withDefault 1 (List.maximum allIds)
  in
    Debug.log "hoho" (max + 1)

extractId : Input -> Int
extractId inp =
  case inp of
    TextInput (id, _, _, _) -> id
    TextArea (id, _, _, _, _) -> id
    Select (id) -> id
    Multiselect (id) -> id
    FileUpload (id) -> id
    Radio (id) -> id
    Checkbox (id) -> id
    Button (id) -> id
