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

textInput : Int -> Input
textInput id =
  { type' = Text
  , id = id
  , classList = [ "form-control" ]
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

textArea : Int -> Input
textArea id =
  { type' = TextArea
  , id = id
  , classList = [ "form-control" ]
  , placeholder = Nothing
  , label = Just "Some label.."
  , disabled = False
  , readonly = False
  , size = Normal
  , addon1 = Nothing
  , addon2 = Nothing
  , small = Nothing
  , rowNumber = 3
  , options = [] }

select : Int -> Input
select id =
  { type' = Select
  , id = id
  , classList = [ "form-control" ]
  , placeholder = Nothing
  , label = Just "Some label.."
  , disabled = False
  , readonly = False
  , size = Normal
  , addon1 = Nothing
  , addon2 = Nothing
  , small = Nothing
  , rowNumber = 1
  , options = [ "options1", "option2", "option3" ] }

multiselect : Int -> Input
multiselect id =
  { type' = Multiselect
  , id = id
  , classList = [ "form-control" ]
  , placeholder = Nothing
  , label = Just "Some label.."
  , disabled = False
  , readonly = False
  , size = Normal
  , addon1 = Nothing
  , addon2 = Nothing
  , small = Nothing
  , rowNumber = 1
  , options = [ "options1", "option2", "option3" ] }

fileUpload : Int -> Input
fileUpload id =
  { type' = FileUpload
  , id = id
  , classList = [ "form-control-file" ]
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

radio : Int -> Input
radio id =
  { type' = Radio
  , id = id
  , classList = [ "form-control" ]
  , placeholder = Nothing
  , label = Just "Some label.."
  , disabled = False
  , readonly = False
  , size = Normal
  , addon1 = Nothing
  , addon2 = Nothing
  , small = Nothing
  , rowNumber = 1
  , options = [ "option1", "option2" ] }

checkbox : Int -> Input
checkbox id =
  { type' = Checkbox
  , id = id
  , classList = [ "form-control" ]
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

button : Int -> Input
button id =
  { type' = Button
  , id = id
  , classList = [ "form-control" ]
  , placeholder = Nothing
  , label = Just "Submit"
  , disabled = False
  , readonly = False
  , size = Normal
  , addon1 = Nothing
  , addon2 = Nothing
  , small = Nothing
  , rowNumber = 1
  , options = [] }

type alias Form = List Input

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
