module FormModel exposing (..)

type alias ClassList = List String
type alias Id = Int
type alias Placeholder = Maybe String
type alias RowNumber = Int
type alias Label = Maybe String
type Size = Small | Normal | Large
type InputType = Text | Search | Email | Url | Tel | Password | Number | DatetimeLocal | Date | Month | Week | Time | Color

type Input
  = TextInput { id: Id, classList: ClassList, placeholder: Placeholder, label: Label, disabled: Bool, readonly: Bool, size: Size, addon1: Maybe String, addon2: Maybe String, small: Maybe String, type': InputType }
  | TextArea { id: Id, classList: ClassList, placeholder: Placeholder, label: Label, rowNumber: RowNumber, disabled: Bool, readonly: Bool }
  | Select { id: Id, classList: ClassList, label: Label, small: Maybe String, disabled: Bool, size: Size, options: List String }
  | Multiselect { id: Id, classList: ClassList, label: Label, small: Maybe String, disabled: Bool, options: List String }
  | FileUpload { id: Id, classList: ClassList, label: Label, disabled: Bool, small: Maybe String }
  | Radio { id: Id, classList: ClassList, label: Label, options: List String }
  | Checkbox { id: Id, classList: ClassList, label: Label }
  | Button { id: Id, classList: ClassList, label: Label }

textInput : Int -> Input
textInput id =
  TextInput { id = id, classList = [ "form-control" ], placeholder = Nothing, label = Just "New input", disabled = False, readonly = False, size = Normal, addon1 = Nothing, addon2 = Nothing, small = Nothing, type' = Text }

textArea : Int -> Input
textArea id =
  TextArea { id = id, classList = [ "form-control" ], placeholder = Nothing, label = Just "New input", rowNumber = 3, disabled = False, readonly = False }

select : Int -> Input
select id =
  Select { id = id, classList = [ "form-control" ], label = Just "New select", small = Nothing, disabled = False, size = Normal, options = ["options1", "option2", "option3"] }

multiselect : Int -> Input
multiselect id =
  Multiselect { id = id, classList = [ "form-control" ], label = Just "New input", disabled = False, options = ["option1", "option2", "option3"], small = Nothing }

fileUpload : Int -> Input
fileUpload id =
  FileUpload { id = id, classList = [ "form-control-file" ], label = Just "New input", disabled = False, small = Nothing }

radio : Int -> Input
radio id =
  Radio { id = id, classList = [ "form-control" ], label = Just "New input", options = ["option1", "option2"] }

checkbox : Int -> Input
checkbox id =
  Checkbox { id = id, classList = [ "form-control" ], label = Just "New input" }

button : Int -> Input
button id =
  Button { id = id, classList = [ "form-control" ], label = Just "New input" }

type alias Form = List Input

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
    Select attrs -> attrs.label
    _ -> Nothing

extractDisabled : Input -> Bool
extractDisabled inp =
  case inp of
    TextInput attrs -> attrs.disabled
    Select attrs -> attrs.disabled
    _ -> False

extractReadonly : Input -> Bool
extractReadonly inp =
  case inp of
    TextInput attrs -> attrs.readonly
    _ -> False

extractSmall : Input -> Maybe String
extractSmall inp =
  case inp of
    TextInput attrs -> attrs.small
    Select attrs -> attrs.small
    _ -> Nothing

extractAddon1 : Input -> Maybe String
extractAddon1 inp =
  case inp of
    TextInput attrs -> attrs.addon1
    _ -> Nothing

extractAddon2 : Input -> Maybe String
extractAddon2 inp =
  case inp of
    TextInput attrs -> attrs.addon2
    _ -> Nothing

extractSize : Input -> Size
extractSize inp =
  case inp of
    TextInput attrs -> attrs.size
    Select attrs -> attrs.size
    _ -> Normal

extractType : Input -> InputType
extractType inp =
  case inp of
    TextInput attrs -> attrs.type'
    _ -> Text

extractOptions : Input -> List String
extractOptions inp =
  case inp of
    Select attrs -> attrs.options
    _ -> []

typeToText : InputType -> String
typeToText type' =
  case type' of
    Text -> "text"
    Search -> "search"
    Email -> "email"
    Url -> "url"
    Tel -> "tel"
    Password -> "password"
    Number -> "number"
    DatetimeLocal -> "datetime-local"
    Date -> "date"
    Month -> "month"
    Week -> "week"
    Time -> "time"
    Color -> "color"
