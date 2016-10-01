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
  | TextArea { id: Id, classList: ClassList, placeholder: Placeholder, label: Label, rowNumber: RowNumber }
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
    textArea = TextArea { id = 2, classList = [ "form-control" ], placeholder = Just "Some placeholder...", label = (Just "Some area"), rowNumber = 3 }
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
    TextArea attrs -> textAreaToHtmlTree
    Select attrs -> selectToHtmlTree
    Multiselect attrs -> multiselectToHtmlTree
    FileUpload attrs -> fileUploadToHtmlTree
    Radio attrs -> radioToHtmlTree
    Checkbox attrs -> checkboxToHtmlTree
    Button attrs -> buttonToHtmlTree

textInputToHtmlTree inp attrs =
  let
    labelFunc = (\value -> Element "label" [Attribute "for" "input1"] (Children []) "Input1")
    inputLabel = Maybe.map labelFunc attrs.label

    smallFunc = (\value -> Element "small" [Attribute "class" "form-text text-muted"] (Children []) value)
    smallText = Maybe.map smallFunc attrs.small

    links = Just (editAndRemoveLink2 inp)

    addFunc = (\value -> Element "div" [Attribute "class" "input-group-addon"] (Children []) value)
    add1 = Maybe.map addFunc attrs.addon1
    add2 = Maybe.map addFunc attrs.addon2

    id = Just (Attribute "id" ("input" ++ toString attrs.id))
    placeholder = Maybe.map (Attribute "placeholder") attrs.placeholder
    disabled = if attrs.disabled then Just (Attribute "disabled" "disabled") else Nothing
    readonly = if attrs.readonly then Just (Attribute "readonly" "readonly") else Nothing
    sizeClass =
      case attrs.size of
        Small -> "form-control-sm"
        Normal -> ""
        Large -> "form-control-lg"
    inputClasses = Just (Attribute "class" (String.join " " (sizeClass::attrs.classList)))

    inputType = Just (Attribute "type" (typeToText attrs.type'))
    inputAttrs = [ id, inputClasses, placeholder, inputType, readonly, disabled ] |> List.filterMap identity

    input1 = Just (Element "input" inputAttrs (Children []) "")
    input =
      if ((List.filterMap identity [add1, add2]) |> List.length) > 0 then
        Just (Element "div" [Attribute "class" "input-group"] (Children ([add1, input1, add2] |> List.filterMap identity)) "")
      else
        input1
  in
    Element "div" [Attribute "class" "form-group"] (Children ([ inputLabel, input, smallText, links ] |> List.filterMap identity)) ""


editAndRemoveLink2 inp =
  let
    l1 = Element "a" [Attribute "href" "javascript:void(0);"] (Children []) "Edit"
    l2 = Element "a" [Attribute "href" "javascript:void(0);"] (Children []) "Remove"
    l3 = Element "a" [Attribute "href" "javascript:void(0);"] (Children []) "Move up"
    l4 = Element "a" [Attribute "href" "javascript:void(0);"] (Children []) "Move down"
    separator = Element "span" [] (Children []) " | "
  in
    -- Element "div" [Attribute "class" "edit-and-remove-link"] (Children [l1, separator, l2, separator, l3, separator, l4]) ""
    Element "editLinks" [] (Children []) (toString (extractId inp))

textAreaToHtmlTree =
  let
    label = Element "label" [Attribute "for" "input1"] (Children []) "Input1"
    inputAttrs = [Attribute "type" "text", Attribute "class" "form-control", Attribute "id" "input1"]
    input = Element "input" inputAttrs (Children []) ""
  in
    Element "div" [Attribute "class" "form-group"] (Children [label, input]) ""

selectToHtmlTree =
  let
    label = Element "label" [Attribute "for" "input1"] (Children []) "Input1"
    inputAttrs = [Attribute "type" "text", Attribute "class" "form-control", Attribute "id" "input1"]
    input = Element "input" inputAttrs (Children []) ""
  in
    Element "div" [Attribute "class" "form-group"] (Children [label, input]) ""

multiselectToHtmlTree =
  let
    label = Element "label" [Attribute "for" "input1"] (Children []) "Input1"
    inputAttrs = [Attribute "type" "text", Attribute "class" "form-control", Attribute "id" "input1"]
    input = Element "input" inputAttrs (Children []) ""
  in
    Element "div" [Attribute "class" "form-group"] (Children [label, input]) ""

fileUploadToHtmlTree =
  let
    label = Element "label" [Attribute "for" "input1"] (Children []) "Input1"
    inputAttrs = [Attribute "type" "text", Attribute "class" "form-control", Attribute "id" "input1"]
    input = Element "input" inputAttrs (Children []) ""
  in
    Element "div" [Attribute "class" "form-group"] (Children [label, input]) ""

radioToHtmlTree =
  let
    label = Element "label" [Attribute "for" "input1"] (Children []) "Input1"
    inputAttrs = [Attribute "type" "text", Attribute "class" "form-control", Attribute "id" "input1"]
    input = Element "input" inputAttrs (Children []) ""
  in
    Element "div" [Attribute "class" "form-group"] (Children [label, input]) ""

checkboxToHtmlTree =
  let
    label = Element "label" [Attribute "for" "input1"] (Children []) "Input1"
    inputAttrs = [Attribute "type" "text", Attribute "class" "form-control", Attribute "id" "input1"]
    input = Element "input" inputAttrs (Children []) ""
  in
    Element "div" [Attribute "class" "form-group"] (Children [label, input]) ""

buttonToHtmlTree =
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
