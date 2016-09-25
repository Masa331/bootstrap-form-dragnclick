module InputUpdate exposing (..)

import Messages exposing (..)
import Models exposing (..)

update msg model =
  case msg of
    PlaceholderEdit placeholder ->
      case model.currentlyEdditedInputId of
        Nothing ->
          (model, Cmd.none)
        Just id ->
          updatePlaceholder id model placeholder
    NoOp ->
      (model, Cmd.none)

-------------
-- Private --
-------------

updatePlaceholder id model newPlaceholder =
  let
    input = List.head (List.filter (\el -> extractId el == id) model.form)
    newInputs = List.map (\inp -> if extractId inp == id then updateInputPlaceholder inp newPlaceholder else inp) model.form
  in
    ({ model | form = newInputs }, Cmd.none)

updateInputPlaceholder inp newPlaceholder =
  case inp of
    TextInput attrs ->
      TextInput { attrs | placeholder = Just newPlaceholder }
    _ -> inp
