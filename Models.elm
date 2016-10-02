module Models exposing (..)

import String exposing (..)

type alias Attribute = { name: String, value: String }
type Children = Children (List Element)
type alias Element = { tag: String, attributes: List Attribute, children: Children, value: String }

type alias ClassList = List String
type alias Id = Int
type alias Placeholder = Maybe String
type alias RowNumber = Int
type alias Label = Maybe String
type Size = Small | Normal | Large
type InputType = Text | Search | Email | Url | Tel | Password | Number | DatetimeLocal | Date | Month | Week | Time | Color

type Input
  = TextInput { id: Id, classList: ClassList, placeholder: Placeholder, label: Label, disabled: Bool, readonly: Bool, size: Size, addon1: Maybe String, addon2: Maybe String, small: Maybe String, type': InputType }
  | TextArea { id: Id, classList: ClassList, placeholder: Placeholder, label: Label, rowNumber: RowNumber, disabled: Bool }
  | Select { id: Id, classList: ClassList, label: Label, small: Maybe String, disabled: Bool, size: Size, options: List String }
  | Multiselect { id: Id, classList: ClassList, label: Label }
  | FileUpload { id: Id, classList: ClassList, label: Label }
  | Radio { id: Id, classList: ClassList, label: Label }
  | Checkbox { id: Id, classList: ClassList, label: Label }
  | Button { id: Id, classList: ClassList, label: Label }

type alias Form = List Input
type alias Model = { form: Form, currentlyEdditedInputId: Maybe Int, newOption: String }

new : Model
new =
  let
    textInput = TextInput { id = 1, classList = [ "form-control" ], placeholder = Nothing, label = (Just "Some input"), disabled = False, readonly = False, size = Normal, addon1 = Nothing, addon2 = Nothing, small = Just "haha", type' = Text }
    textArea = TextArea { id = 2, classList = [ "form-control" ], placeholder = Just "Some placeholder...", label = (Just "Some area"), rowNumber = 3, disabled = False }
    checkbox = Checkbox { id = 3, classList = [  ], label = (Just "Some area") }
    select1 = Select { id = 4, classList = [ "form-control" ], label = (Just "Some select"), small = Nothing, disabled = False, size = Normal, options = ["options1", "option2", "option3"] }
    button = Button { id = 5, classList = [ "form-control" ], label = (Just "Some area") }
  in
    Model [ textInput, textArea, select1, checkbox, button ] Nothing ""

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



-- WIP ----------------------------

modelToHtmlTree : Form -> Element
modelToHtmlTree form =
  let
    children = List.map inputToHtmlTree form
  in
    Element "form" [] (Children children) ""

inputToHtmlTree input =
  case input of
    TextInput attrs -> textInputToHtmlTree input attrs
    TextArea attrs -> textAreaToHtmlTree input attrs
    Select attrs -> selectToHtmlTree input attrs
    Multiselect attrs -> multiselectToHtmlTree input attrs
    FileUpload attrs -> fileUploadToHtmlTree input attrs
    Radio attrs -> radioToHtmlTree input attrs
    Checkbox attrs -> checkboxToHtmlTree input attrs
    Button attrs -> buttonToHtmlTree input attrs

textInputToHtmlTree inp attrs =
  let
    id = toId attrs.id
    placeholder = toPlaceholder attrs.placeholder
    disabled = toDisabled attrs.disabled
    readonly = toReadonly attrs.readonly
    inputClasses = toClasses attrs.classList attrs.size
    inputType = toType attrs.type'
    inputAttrs =
      [ id, inputClasses, placeholder, inputType, readonly, disabled ]
      |> List.filterMap identity

    inputLabel = toLabel attrs.label
    smallText = toSmall attrs.small
    links = toLinks (extractId inp)
    add1 = toAddon attrs.addon1
    add2 = toAddon attrs.addon2
    input1 = Just (Element "input" inputAttrs (Children []) "")
    input =
      if List.isEmpty (List.filterMap identity [add1, add2]) then
        input1
      else
        Just (Element "div" [Attribute "class" "input-group"] (Children ([add1, input1, add2] |> List.filterMap identity)) "")

    children = [ inputLabel, input, smallText, links ] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-group"] (Children children) ""

toPlaceholder : Maybe String -> Maybe Attribute
toPlaceholder value =
  Maybe.map (Attribute "placeholder") value

toId : Int -> Maybe Attribute
toId value =
  Just (Attribute "id" ("input" ++ toString value))

toDisabled : Bool -> Maybe Attribute
toDisabled value =
  if value then Just (Attribute "disabled" "disabled") else Nothing

toReadonly : Bool -> Maybe Attribute
toReadonly value =
  if value then Just (Attribute "readonly" "readonly") else Nothing

toAddon : Maybe String -> Maybe Element
toAddon value =
  Maybe.map (\value -> Element "div" [Attribute "class" "input-group-addon"] (Children []) value) value

toSmall : Maybe String -> Maybe Element
toSmall value =
  Maybe.map (\value -> Element "small" [Attribute "class" "form-text text-muted"] (Children []) value) value

toLabel : Maybe String -> Maybe Element
toLabel value =
  Maybe.map (\value -> Element "label" [Attribute "for" "input1"] (Children []) "Input1") value

toType : InputType -> Maybe Attribute
toType value =
  Just (Attribute "type" (typeToText value))

toLinks : Id -> Maybe Element
toLinks value =
  Just (Element "editLinks" [] (Children []) (toString value))

toClasses : List String -> Size -> Maybe Attribute
toClasses classList size =
  let
    sizeClass =
      case size of
        Small -> "form-control-sm"
        Normal -> ""
        Large -> "form-control-lg"
  in
    Just (Attribute "class" (String.join " " (sizeClass::classList)))

selectToHtmlTree inp attrs =
  let
    id = toId attrs.id
    disabled = toDisabled attrs.disabled
    inputClasses = toClasses attrs.classList attrs.size
    selectAttrs = [ id, inputClasses, disabled ] |> List.filterMap identity

    options = List.map (\value -> Element "option" [] (Children []) value) attrs.options
    inputLabel = toLabel attrs.label
    smallText = toSmall attrs.small
    links = toLinks (extractId inp)
    select = Just (Element "select" selectAttrs (Children options) "")
    children = [inputLabel, select, smallText, links] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-group"] (Children children) ""

textAreaToHtmlTree inp attrs =
  let
    inputLabel = toLabel attrs.label
    id = toId attrs.id
    disabled = toDisabled attrs.disabled
    placeholder = toPlaceholder attrs.placeholder
    inputClasses = toClasses attrs.classList Normal
    rowNumber = Just (Attribute "rows" (toString attrs.rowNumber))
    links = toLinks (extractId inp)

    areaAttrs = [ id, inputClasses, disabled, placeholder, rowNumber ] |> List.filterMap identity
    -- areaAttrs = [ id, inputClasses, disabled, placeholder, rowNumber ] |> List.filterMap identity
    area = Just (Element "textarea" areaAttrs (Children []) "")
  in
    Element "div" [Attribute "class" "form-group"] (Children ([inputLabel, area, links] |> List.filterMap identity)) ""

multiselectToHtmlTree inp attrs =
  let
    label = Element "label" [Attribute "for" "input1"] (Children []) "Input1"
    inputAttrs = [Attribute "type" "text", Attribute "class" "form-control", Attribute "id" "input1"]
    input = Element "input" inputAttrs (Children []) ""
  in
    Element "div" [Attribute "class" "form-group"] (Children [label, input]) ""

fileUploadToHtmlTree inp attrs =
  let
    label = Element "label" [Attribute "for" "input1"] (Children []) "Input1"
    inputAttrs = [Attribute "type" "text", Attribute "class" "form-control", Attribute "id" "input1"]
    input = Element "input" inputAttrs (Children []) ""
  in
    Element "div" [Attribute "class" "form-group"] (Children [label, input]) ""

radioToHtmlTree inp attrs =
  let
    label = Element "label" [Attribute "for" "input1"] (Children []) "Input1"
    inputAttrs = [Attribute "type" "text", Attribute "class" "form-control", Attribute "id" "input1"]
    input = Element "input" inputAttrs (Children []) ""
  in
    Element "div" [Attribute "class" "form-group"] (Children [label, input]) ""

checkboxToHtmlTree inp attrs =
  let
    label = Element "label" [Attribute "for" "input1"] (Children []) "Input1"
    inputAttrs = [Attribute "type" "text", Attribute "class" "form-control", Attribute "id" "input1"]
    input = Element "input" inputAttrs (Children []) ""
  in
    Element "div" [Attribute "class" "form-group"] (Children [label, input]) ""

buttonToHtmlTree inp attrs =
  let
    label = Element "label" [Attribute "for" "input1"] (Children []) "Input1"
    inputAttrs = [Attribute "type" "text", Attribute "class" "form-control", Attribute "id" "input1"]
    input = Element "input" inputAttrs (Children []) ""
  in
    Element "div" [Attribute "class" "form-group"] (Children [label, input]) ""

isVoid element =
  List.member element.tag voidElementsList

voidElementsList =
  ["area", "base", "br", "col", "command", "embed", "hr", "img", "input", "keygen", "link", "meta", "param", "source", "track", "wbr"]

isDeletable model =
  if model.tag == "editLinks" then
    True
  else
    False
