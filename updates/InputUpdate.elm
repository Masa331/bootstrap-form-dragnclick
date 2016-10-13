module InputUpdate exposing (..)

import Messages exposing (..)
import Models exposing (..)
import FormModel exposing (..)

update : InputMsg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PlaceholderEdit newPlaceholder ->
      updateInputAttribute placeholderUpdateFunc model newPlaceholder
    LabelEdit newLabel ->
      updateInputAttribute labelUpdateFunc model newLabel
    SmallEdit newLabel ->
      updateInputAttribute smallUpdateFunc model newLabel
    DisabledEdit newDisabled ->
      updateInputAttribute disabledUpdateFunc model newDisabled
    ReadonlyEdit newDisabled ->
      updateInputAttribute readonlyUpdateFunc model newDisabled
    FirstAddonEdit newAddon ->
      updateInputAttribute firstAddonEditFunc model newAddon
    SecondAddonEdit newAddon ->
      updateInputAttribute secondAddonEditFunc model newAddon
    SizeEdit newSize ->
      updateInputAttribute sizeEditFunc model newSize
    TypeEdit newType ->
      updateInputAttribute typeEditFunc model newType
    RowNumberEdit newRowNumber ->
      updateInputAttribute rowNumberEditFunc model newRowNumber
    NewOptionEdit value ->
      ({ model | newOption = value }, Cmd.none)
    SaveNewOption ->
      updateInputAttribute addNewOptionFunc model model.newOption
    RemoveOption value ->
      updateInputAttribute removeOptionFunc model value

-------------
-- Private --
-------------

-- updateInputAttribute : (Input -> String -> Input) -> Model -> Int -> String -> (Model, Cmd Msg)
-- updateInputAttribute : (Input -> a -> Input) -> Model -> Int -> String -> (Model, Cmd Msg)
updateInputAttribute updateFunc model newPlaceholder =
  case currentlyEdditedInput model of
    Nothing ->
      (model, Cmd.none)
    Just input ->
      let
        updatedInput = updateFunc input newPlaceholder
        newInputs = List.map (\inp -> if inp.id == input.id then updatedInput else inp) model.form
      in
        ({ model | form = newInputs }, Cmd.none)

removeOptionFunc : Input -> String -> Input
removeOptionFunc inp newOption =
  let
    func = (\neco -> if newOption == neco then Nothing else Just neco)
  in
    { inp | options = List.map func inp.options |> List.filterMap identity }

addNewOptionFunc : Input -> String -> Input
addNewOptionFunc inp newOption =
  { inp | options = newOption::inp.options }

sizeEditFunc : Input -> String -> Input
sizeEditFunc inp newSize =
  let
    neco = textToSize newSize
  in
    { inp | size = neco }

rowNumberEditFunc : Input -> RowNumber -> Input
rowNumberEditFunc inp newRowNumber =
  let
    neco = newRowNumber
  in
    { inp | rowNumber = neco }

typeEditFunc : Input -> String -> Input
typeEditFunc inp newType =
  let
    neco = textToType newType
  in
    { inp | type' = neco }

firstAddonEditFunc : Input -> String -> Input
firstAddonEditFunc inp newAddon =
  let
    wrappedAddon = if newAddon == "" then Nothing else Just newAddon
  in
    { inp | addon1 = wrappedAddon }

secondAddonEditFunc : Input -> String -> Input
secondAddonEditFunc inp newAddon =
  let
    wrappedAddon = if newAddon == "" then Nothing else Just newAddon
  in
    { inp | addon2 = wrappedAddon }

placeholderUpdateFunc : Input -> String -> Input
placeholderUpdateFunc inp newPlaceholder =
  { inp | placeholder = Just newPlaceholder }

labelUpdateFunc : Input -> String -> Input
labelUpdateFunc inp newLabel =
  { inp | label = Just newLabel }

smallUpdateFunc : Input -> String -> Input
smallUpdateFunc inp newSmall =
  { inp | small = Just newSmall }

disabledUpdateFunc : Input -> Bool -> Input
disabledUpdateFunc inp newDisabled =
  { inp | disabled = newDisabled }

readonlyUpdateFunc : Input -> Bool -> Input
readonlyUpdateFunc inp newReadonly =
  { inp | readonly = newReadonly }
