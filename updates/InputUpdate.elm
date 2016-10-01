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
          updateInputAttribute disabledUpdateFunc model inputId newDisabled
        ReadonlyEdit newDisabled ->
          updateInputAttribute readonlyUpdateFunc model inputId newDisabled
        FirstAddonEdit newAddon ->
          updateInputAttribute firstAddonEditFunc model inputId newAddon
        SecondAddonEdit newAddon ->
          updateInputAttribute secondAddonEditFunc model inputId newAddon
        SizeEdit newSize ->
          updateInputAttribute sizeEditFunc model inputId newSize
        TypeEdit newType ->
          updateInputAttribute typeEditFunc model inputId newType
        NewOptionEdit value ->
          ({ model | newOption = value }, Cmd.none)
        SaveNewOption ->
          updateInputAttribute addNewOptionFunc model inputId model.newOption
        RemoveOption value ->
          updateInputAttribute removeOptionFunc model inputId value

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

removeOptionFunc : Input -> String -> Input
removeOptionFunc inp newOption =
  let
    func = (\neco -> if newOption == neco then Nothing else Just neco)
  in
  case inp of
    Select attrs ->
      Select { attrs | options = List.map func attrs.options |> List.filterMap identity }
    _ -> inp

addNewOptionFunc : Input -> String -> Input
addNewOptionFunc inp newOption =
  case inp of
    Select attrs ->
      Select { attrs | options = newOption::attrs.options }
    _ -> inp

sizeEditFunc : Input -> String -> Input
sizeEditFunc inp newSize =
  let
    neco =
      case newSize of
        "small" -> Small
        "normal" -> Normal
        "large" -> Large
        _ -> Normal
  in
    case inp of
      TextInput attrs ->
        TextInput { attrs | size = neco }
      Select attrs ->
        Select { attrs | size = neco }
      _ -> inp

typeEditFunc : Input -> String -> Input
typeEditFunc inp newType =
  let
    neco =
      case newType of
        "text" -> Text
        "search" -> Search
        "email" -> Email
        "url" -> Url
        "tel" -> Tel
        "password" -> Password
        "number" -> Number
        "datetime-local" -> DatetimeLocal
        "date" -> Date
        "month" -> Month
        "week" -> Week
        "time" -> Time
        "color" -> Color
        _ -> Text
  in
    case inp of
      TextInput attrs ->
        TextInput { attrs | type' = neco }
      _ -> inp

firstAddonEditFunc : Input -> String -> Input
firstAddonEditFunc inp newAddon =
  let
    wrappedAddon = if newAddon == "" then Nothing else Just newAddon
  in
    case inp of
      TextInput attrs ->
        TextInput { attrs | addon1 = wrappedAddon }
      _ -> inp

secondAddonEditFunc : Input -> String -> Input
secondAddonEditFunc inp newAddon =
  let
    wrappedAddon = if newAddon == "" then Nothing else Just newAddon
  in
    case inp of
      TextInput attrs ->
        TextInput { attrs | addon2 = wrappedAddon }
      _ -> inp

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
    Select attrs ->
      Select { attrs | label = Just newLabel }
    _ -> inp

smallUpdateFunc : Input -> String -> Input
smallUpdateFunc inp newSmall =
  case inp of
    TextInput attrs ->
      TextInput { attrs | small = Just newSmall }
    Select attrs ->
      Select { attrs | small = Just newSmall }
    _ -> inp

disabledUpdateFunc : Input -> Bool -> Input
disabledUpdateFunc inp newDisabled =
  case inp of
    TextInput attrs ->
      TextInput { attrs | disabled = newDisabled }
    Select attrs ->
      Select { attrs | disabled = newDisabled }
    _ -> inp

readonlyUpdateFunc : Input -> Bool -> Input
readonlyUpdateFunc inp newReadonly =
  case inp of
    TextInput attrs ->
      TextInput { attrs | readonly = newReadonly }
    _ -> inp
