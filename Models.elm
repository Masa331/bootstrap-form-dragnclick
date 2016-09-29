module Models exposing (..)

import String exposing (..)

type alias ClassList = List String
type alias Id = Int
type alias Placeholder = Maybe String
type alias RowNumber = Int
type alias Label = Maybe String
type Size = Small | Normal | Big

type Input
  = TextInput { id: Id, classList: ClassList, placeholder: Placeholder, label: Label, disabled: Bool, readonly: Bool, size: Size, addon1: Maybe String, addon2: Maybe String }
  | TextArea { id: Id, classList: ClassList, placeholder: Placeholder, label: Label, rowNumber: RowNumber }
  | Select { id: Id, classList: ClassList, label: Label }
  | Multiselect { id: Id, classList: ClassList, label: Label }
  | FileUpload { id: Id, classList: ClassList, label: Label }
  | Radio { id: Id, classList: ClassList, label: Label }
  | Checkbox { id: Id, classList: ClassList, label: Label }
  | Button { id: Id, classList: ClassList, label: Label }

type alias Form = List Input
type alias Model = { form: Form, currentlyEdditedInputId: Maybe Int }

new : Model
new =
  let
    textInput = TextInput { id = 1, classList = [ "form-control" ], placeholder = Nothing, label = (Just "Some input"), disabled = False, readonly = False, size = Normal, addon1 = Nothing, addon2 = Nothing }
    textArea = TextArea { id = 2, classList = [ "form-control" ], placeholder = Just "Some placeholder...", label = (Just "Some area"), rowNumber = 3 }
    checkbox = Checkbox { id = 2, classList = [  ], label = (Just "Some area") }
    button = Button { id = 2, classList = [ "form-control" ], label = (Just "Some area") }
  in
    Model [ textInput, textArea, checkbox, button ] Nothing

extractId : Input -> Int
extractId inp =
  case inp of
    TextInput attrs -> attrs.id
    TextArea attrs -> attrs.id
    Select attrs -> attrs.id
    Multiselect attrs -> attrs.id
    FileUpload attrs -> attrs.id
    Radio attrs -> attrs.id
    Checkbox attrs -> attrs.id
    Button attrs -> attrs.id

extractPlaceholder : Input -> Maybe String
extractPlaceholder inp =
  case inp of
    TextInput attrs -> attrs.placeholder
    _ -> Nothing

extractLabel : Input -> Maybe String
extractLabel inp =
  case inp of
    TextInput attrs -> attrs.label
    _ -> Nothing
