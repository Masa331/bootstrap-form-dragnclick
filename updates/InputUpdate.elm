module InputUpdate exposing (..)

import Messages exposing (..)
import Models exposing (..)

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
          updateInputAttribute labelUpdateFunc model inputId newLabel
        SmallEdit newLabel ->
          updateInputAttribute smallUpdateFunc model inputId newLabel
        DisabledEdit newDisabled ->
          -- (model, Cmd.none)
          updateInputAttribute disabledUpdateFunc model inputId newDisabled
        NoOp ->
          (model, Cmd.none)

-------------
-- Private --
-------------

-- updateInputAttribute : (Input -> String -> Input) -> Model -> Int -> String -> (Model, Cmd Msg)
-- updateInputAttribute : (Input -> a -> Input) -> Model -> Int -> String -> (Model, Cmd Msg)
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

labelUpdateFunc : Input -> String -> Input
labelUpdateFunc inp newLabel =
  case inp of
    TextInput attrs ->
      TextInput { attrs | label = Just newLabel }
    _ -> inp

smallUpdateFunc : Input -> String -> Input
smallUpdateFunc inp newSmall =
  case inp of
    TextInput attrs ->
      TextInput { attrs | small = Just newSmall }
    _ -> inp

disabledUpdateFunc : Input -> Bool -> Input
disabledUpdateFunc inp newDisabled =
  case inp of
    TextInput attrs ->
      TextInput { attrs | disabled = newDisabled }
    _ -> inp
