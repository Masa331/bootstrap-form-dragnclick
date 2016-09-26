module InputUpdate exposing (..)

import Messages exposing (..)
import Models exposing (..)
--
-- update msg model =
--   case msg of
--     PlaceholderEdit placeholder ->
--       case model.currentlyEdditedInputId of
--         Nothing ->
--           (model, Cmd.none)
--         Just id ->
--           updatePlaceholder id model placeholder
--     LabelEdit newLabel ->
--       (model, Cmd.none)
--     NoOp ->
--       (model, Cmd.none)

update : InputMsg -> Model -> (Model, Cmd Msg)
update msg model =
  case model.currentlyEdditedInputId of
    Nothing ->
      (model, Cmd.none)
    Just inputId ->
      case msg of
        PlaceholderEdit newPlaceholder ->
          updateInputAttribute placeholderUpdateFunc model inputId newPlaceholder
        LabelEdit newLabel ->
          (model, Cmd.none)
        NoOp ->
          (model, Cmd.none)

-------------
-- Private --
-------------

updateInputAttribute : (Input -> String -> Input) -> Model -> Int -> String -> (Model, Cmd Msg)
updateInputAttribute updateFunc model inputId newPlaceholder =
  let
    newInputs = List.map (\inp -> if extractId inp == inputId then updateFunc inp newPlaceholder else inp) model.form
  in
    ({ model | form = newInputs }, Cmd.none)

placeholderUpdateFunc : Input -> String -> Input
placeholderUpdateFunc inp newPlaceholder =
  case inp of
    TextInput attrs ->
      TextInput { attrs | placeholder = Just newPlaceholder }
    _ -> inp
