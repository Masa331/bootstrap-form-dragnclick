module InputUpdate exposing (..)

import Messages exposing (..)
import Models exposing (..)
import FormModel exposing (..)

update : InputMsg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PlaceholderEdit id newPlaceholder ->
      updateInput model id (updatePlaceholder newPlaceholder)
    LabelEdit id newLabel ->
      updateInput model id (updateLabel newLabel)
    SmallEdit id newSmall ->
      updateInput model id (updateSmall newSmall)
    ToggleDisabled id ->
      updateInput model id (toggleDisabled)
    FirstAddonEdit id newAddon ->
      updateInput model id (updateFirstAddon newAddon)
    SecondAddonEdit id newAddon ->
      updateInput model id (updateSecondAddon newAddon)
    SizeEdit id newSize ->
      updateInput model id (updateSize newSize)
    TypeEdit id newType ->
      updateInput model id (updateType newType)
    RowNumberEdit id newRowNumber ->
      updateInput model id (updateRowNumber newRowNumber)
    NewOptionEdit value ->
      ({ model | newOption = value }, Cmd.none)
    SaveNewOption id ->
      updateInput model id (addNewOption model.newOption)
    RemoveOption id value ->
      updateInput model id (removeOption value)

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
  { input | options = newOption::input.options }

updateSize : String -> Input -> Input
updateSize newSize input =
  { input | size = textToSize newSize }

updateRowNumber : RowNumber -> Input -> Input
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
