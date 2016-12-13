module Inputs exposing (..)

import String
import ElementMap

type Size = Small | Normal | Large
type InputType = Text | TextArea | Select | Multiselect | FileUpload | Radio | Checkbox | Button | Email | Url | Password | Number | DatetimeLocal | Date | Time | Color

type alias Input =
  { type_: InputType
  , id: Int
  , placeholder: Maybe String
  , label: Maybe String
  , disabled: Bool
  , size: Size
  , addon1: Maybe String
  , addon2: Maybe String
  , small: Maybe String
  , rowNumber: String
  , dragged: Bool
  , dimensions: Maybe ElementMap.ElementDimensions
  , options: List String }

blankInput : Input
blankInput =
  { type_ = Text
  , id = 0
  , placeholder = Nothing
  , label = Just "Some label.."
  , disabled = False
  , size = Normal
  , addon1 = Nothing
  , addon2 = Nothing
  , small = Nothing
  , rowNumber = "1"
  , dragged = False
  , dimensions = Nothing
  , options = [] }

textInput : Input
textInput =
  { blankInput | type_ = Text }

checkbox : Input
checkbox =
  { blankInput | type_ = Checkbox }

button : Input
button =
  { blankInput | type_ = Button }

inputTypeToString : InputType -> String
inputTypeToString type_ =
  case type_ of
    DatetimeLocal -> "datetime-local"
    _ -> (toString >> String.toLower) type_

rowsToNumber : String -> Int
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
    "email" -> Email
    "url" -> Url
    "password" -> Password
    "number" -> Number
    "datetime-local" -> DatetimeLocal
    "date" -> Date
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

updateInputs inputs targetId updateFunc =
  List.map (\inp -> if inp.id == targetId then updateFunc inp else inp) inputs
