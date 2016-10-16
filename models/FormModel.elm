module FormModel exposing (..)

import String

type alias ClassList = List String
type alias Id = Int
type alias Placeholder = Maybe String
type alias RowNumber = String
type alias Label = Maybe String
type Size = Small | Normal | Large

type InputType = Text | TextArea | Select | Multiselect | FileUpload | Radio | Checkbox | Button | Search | Email | Url | Tel | Password | Number | DatetimeLocal | Date | Month | Week | Time | Color

type alias Input =
  { type': InputType
  , id: Id
  , classList: ClassList
  , placeholder: Placeholder
  , label: Label
  , disabled: Bool
  , readonly: Bool
  , size: Size
  , addon1: Maybe String
  , addon2: Maybe String
  , small: Maybe String
  , rowNumber: RowNumber
  , dragged: Bool
  , options: List String }

type alias Form = List Input

blankInput : Input
blankInput =
  { type' = Text
  , id = 0
  , classList = []
  , placeholder = Nothing
  , label = Just "Some label.."
  , disabled = False
  , readonly = False
  , size = Normal
  , addon1 = Nothing
  , addon2 = Nothing
  , small = Nothing
  , rowNumber = "1"
  , dragged = False
  , options = [] }

textInput : Input
textInput =
  { blankInput | type' = Text }

textArea : Input
textArea =
  { blankInput | type' = TextArea, rowNumber = "3" }

select : Input
select =
  { blankInput | type' = Select, options = [ "options1", "option2", "option3" ] }

multiselect : Input
multiselect =
  { blankInput | type' = Multiselect, options = [ "options1", "option2", "option3" ] }

fileUpload : Input
fileUpload =
  { blankInput | type' = FileUpload, classList = [ "form-control-file" ] }

radio : Input
radio =
  { blankInput | type' = Radio, options = [ "options1", "option2", "option3" ] }

checkbox : Input
checkbox =
  { blankInput | type' = Checkbox, classList = [ "form-control" ] }

button :Input
button =
  { blankInput | type' = Button }

inputTypeToString : InputType -> String
inputTypeToString type' =
  case type' of
    DatetimeLocal -> "datetime-local"
    _ -> (toString >> String.toLower) type'

sizeToString : Size -> String
sizeToString =
  toString >> String.toLower

rowsToNumber : RowNumber -> Int
rowsToNumber rowNumber =
  String.toInt rowNumber
    |> Result.withDefault 3

textToType : String -> InputType
textToType text =
  case text of
    "text" -> Text
    "textarea" -> TextArea
    "select" -> Select
    "multiselect" -> Multiselect
    "fileupload" -> FileUpload
    "radio" -> Radio
    "checkbox" -> Checkbox
    "button" -> Button
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

textToSize : String -> Size
textToSize text =
  case text of
    "small" -> Small
    "normal" -> Normal
    "large" -> Large
    _ -> Normal

stringInputTypes : List String
stringInputTypes =
  [ "text"
  , "textarea"
  , "select"
  , "multiselect"
  , "fileupload"
  , "radio"
  , "checkbox"
  , "button"
  , "search"
  , "email"
  , "url"
  , "tel"
  , "password"
  , "number"
  , "datetime-local"
  , "date"
  , "month"
  , "week"
  , "time"
  , "color"
  ]

updateInput model id updateFunc =
  let
    updatedInputs = List.map (\inp -> if inp.id == id then updateFunc inp else inp) model.form
  in
    ({ model | form = updatedInputs }, Cmd.none)
