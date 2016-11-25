module InputUpdate exposing (..)

import Messages exposing (..)
import Models exposing (..)
import FormModel exposing (..)

update : InputMsg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PlaceholderEdit id newPlaceholder ->
      ({ model | inputs = updateInputs model.inputs id (updatePlaceholder newPlaceholder) }, Cmd.none)
    LabelEdit id newLabel ->
      ({ model | inputs = updateInputs model.inputs id (updateLabel newLabel) }, Cmd.none)
    SmallEdit id newSmall ->
      ({ model | inputs = updateInputs model.inputs id (updateSmall newSmall) }, Cmd.none)
    ToggleDisabled id ->
      ({ model | inputs = updateInputs model.inputs id (toggleDisabled) }, Cmd.none)
    FirstAddonEdit id newAddon ->
      ({ model | inputs = updateInputs model.inputs id (updateFirstAddon newAddon) }, Cmd.none)
    SecondAddonEdit id newAddon ->
      ({ model | inputs = updateInputs model.inputs id (updateSecondAddon newAddon) }, Cmd.none)
    SizeEdit id newSize ->
      ({ model | inputs = updateInputs model.inputs id (updateSize newSize) }, Cmd.none)
    TypeEdit id newType ->
      ({ model | inputs = updateInputs model.inputs id (updateType newType) }, Cmd.none)
    RowNumberEdit id newRowNumber ->
      ({ model | inputs = updateInputs model.inputs id (updateRowNumber newRowNumber) }, Cmd.none)
    NewOptionEdit value ->
      ({ model | newOption = value }, Cmd.none)
    SaveNewOption id ->
      ({ model | inputs = updateInputs model.inputs id (addNewOption model.newOption)
               , newOption = Debug.log "WAT" "" }
      , Cmd.none)
    RemoveOption id value ->
      ({ model | inputs = updateInputs model.inputs id (removeOption value) }, Cmd.none)

-------------
-- Private --
-------------

updateLabel : String -> Input -> Input
updateLabel newLabel input =
  { input | label = Just newLabel }

updatePlaceholder : String -> Input -> Input
updatePlaceholder newPlaceholder input =
  { input | placeholder = Just newPlaceholder }

removeOption : String -> Input -> Input
removeOption newOption inp =
  let
    func = (\neco -> if newOption == neco then Nothing else Just neco)
  in
    { inp | options = List.map func inp.options |> List.filterMap identity }

addNewOption : String -> Input -> Input
addNewOption newOption input =
  { input | options = input.options ++ [newOption] }

updateSize : String -> Input -> Input
updateSize newSize input =
  { input | size = textToSize newSize }

updateRowNumber : String -> Input -> Input
updateRowNumber newRowNumber input =
  { input | rowNumber = newRowNumber }

updateType : String -> Input -> Input
updateType newType input =
  { input | type_ = textToType newType }

updateFirstAddon : String -> Input -> Input
updateFirstAddon newAddon inp =
  let
    wrappedAddon = if newAddon == "" then Nothing else Just newAddon
  in
    { inp | addon1 = wrappedAddon }

updateSecondAddon : String -> Input -> Input
updateSecondAddon newAddon inp =
  let
    wrappedAddon = if newAddon == "" then Nothing else Just newAddon
  in
    { inp | addon2 = wrappedAddon }

updateSmall : String -> Input -> Input
updateSmall newSmall input =
  { input | small = Just newSmall }

toggleDisabled : Input -> Input
toggleDisabled input =
  { input | disabled = not input.disabled }
