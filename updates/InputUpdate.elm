module InputUpdate exposing (..)

import Messages exposing (..)
import Models exposing (..)
import Form exposing (..)
import Inputs exposing (..)

update : InputMsg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    updatedInputs =
      case msg of
        PlaceholderEdit id newPlaceholder ->
          List.map (\input -> if input.id == id then { input | placeholder = Just newPlaceholder } else input) model.inputs
        LabelEdit id newLabel ->
          List.map (\input -> if input.id == id then { input | label = Just newLabel } else input) model.inputs
        SmallEdit id newSmall ->
          List.map (\input -> if input.id == id then { input | small = Just newSmall } else input) model.inputs
        ToggleDisabled id ->
          List.map (\input -> if input.id == id then { input | disabled = not input.disabled } else input) model.inputs
        FirstAddonEdit id newAddon ->
          List.map (\input -> if input.id == id then { input | addon1 = if newAddon == "" then Nothing else Just newAddon } else input) model.inputs
        SecondAddonEdit id newAddon ->
          List.map (\input -> if input.id == id then { input | addon2 = if newAddon == "" then Nothing else Just newAddon } else input) model.inputs
        SizeEdit id newSize ->
          List.map (\input -> if input.id == id then { input | size = textToSize newSize } else input) model.inputs
        TypeEdit id newType ->
          List.map (\input -> if input.id == id then { input | type_ = textToType newType } else input) model.inputs
        RowNumberEdit id newRowNumber ->
          List.map (\input -> if input.id == id then { input | rowNumber = newRowNumber } else input) model.inputs
        SaveNewOption id ->
          List.map (\input -> if input.id == id then { input | options = input.options ++ [model.newOption] } else input) model.inputs
        RemoveOption id value ->
          List.map (\input -> if input.id == id then { input | options = List.filter (\option -> option /= value) input.options } else input) model.inputs
  in
    ({ model | inputs = updatedInputs }, Cmd.none)
