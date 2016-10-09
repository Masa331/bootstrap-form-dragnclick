module FormUpdate exposing (..)

import Array exposing (..)
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
      moveInputUp id model
    MoveDown id ->
      (model, Cmd.none)

moveInputUp : Id -> Model -> (Model, Cmd Msg)
moveInputUp id model =
  let
    index = getIndexOfElementWithId model.form id

    ary = Array.fromList model.form
    -- index = getIndexOfElementWithId ary id
    newForm =
      moveLeft index ary
      |> Array.toList
  in
    ({ model | form = newForm }, Cmd.none)

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

-------------
-- Helpers --
-------------

moveLeft : Int -> Array a -> Array a
moveLeft index a1 =
  let
    movedRight = Array.get (index - 1) a1
    movedLeft = Array.get (index) a1
  in
    replace (index - 1) movedLeft a1
      |> replace index movedRight


moveRight : Int -> Array a -> Array a
moveRight index a1 =
  let
    movedRight = Array.get (index) a1
    movedLeft = Array.get (index + 1) a1
  in
    replace (index + 1) movedRight a1
      |> replace index movedLeft

replace : Int -> Maybe a -> Array a -> Array a
replace index element ary =
  case element of
    Nothing ->
      ary
    Just b ->
      Array.set index b ary


getIndexOfElementWithId : List Input -> Id -> Int
getIndexOfElementWithId lst id =
  helper lst id 0

helper : List Input -> Id -> Int -> Int
helper lst id offset =
  case lst of
    [] ->
      -1
    x :: xs ->
      if x.id == id then
        offset
      else
        helper xs id (offset + 1)
