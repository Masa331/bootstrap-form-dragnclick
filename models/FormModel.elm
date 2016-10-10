module FormModel exposing (..)

type alias ClassList = List String
type alias Id = Int
type alias Placeholder = Maybe String
type alias RowNumber = Int
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
  , rowNumber = 1
  , options = [] }

textInput : Int -> Input
textInput id =
  { blankInput | type' = Text, id = id }

textArea : Int -> Input
textArea id =
  { blankInput | type' = TextArea, id = id, rowNumber = 3 }

select : Int -> Input
select id =
  { blankInput | type' = Select, id = id, options = [ "options1", "option2", "option3" ] }

multiselect : Int -> Input
multiselect id =
  { blankInput | type' = Multiselect, id = id, options = [ "options1", "option2", "option3" ] }

fileUpload : Int -> Input
fileUpload id =
  { blankInput | type' = FileUpload, id = id, classList = [ "form-control-file" ] }

radio : Int -> Input
radio id =
  { blankInput | type' = Radio, id = id, options = [ "options1", "option2", "option3" ] }

checkbox : Int -> Input
checkbox id =
  { blankInput | type' = Checkbox, id = id,  classList = [ "form-control" ] }

button : Int -> Input
button id =
  { blankInput | type' = Button, id = id }

extractType : Input -> InputType
extractType inp =
  inp.type'

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
    _ -> "resolve please"

textToType : String -> InputType
textToType text =
  case text of
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

textToSize : String -> Size
textToSize text =
  case text of
    "small" -> Small
    "normal" -> Normal
    "large" -> Large
    _ -> Normal
