module Models exposing (..)

import String exposing (..)

type alias ClassList = List String
type alias Id = Int
type alias Placeholder = Maybe String
type alias RowNumber = Int
type alias Label = Maybe String

  -- = TextInput (Id, ClassList, Placeholder, Label)
type Input
  = TextInput { id: Id, classList: ClassList, placeholder: Placeholder, label: Label }
  | TextArea { id: Id, classList: ClassList, placeholder: Placeholder, label: Label, rowNumber: RowNumber }
  | Select (Id)
  | Multiselect (Id)
  | FileUpload (Id)
  | Radio (Id)
  | Checkbox (Id)
  | Button (Id)

type alias Form = List Input
type alias Model = { form: Form, currentlyEdditedInputId: Maybe Int }

new : Model
new =
  let
    -- textInput = TextInput (1, [ "form-control" ], Nothing, Just "Some input")
    textInput = TextInput { id = 1, classList = [ "form-control" ], placeholder = Nothing, label = (Just "Some input") }
    -- textInput2 = TextArea (2, [ "form-control" ], (Just "Some placeholder..."), 3, Just "Some textarea")
    textInput2 = TextArea { id = 2, classList = [ "form-control" ], placeholder = Just "Some placeholder...", label = (Just "Some area"), rowNumber = 3 }
  in
    Model [textInput, textInput2] Nothing

extractId : Input -> Int
extractId inp =
  case inp of
    TextInput attrs -> attrs.id
    TextArea attrs -> attrs.id
    Select (id) -> id
    Multiselect (id) -> id
    FileUpload (id) -> id
    Radio (id) -> id
    Checkbox (id) -> id
    Button (id) -> id
