-- module Models exposing (Model, Children, initialModel2)
module Models exposing (..)

import String exposing (..)

type alias Attribute = { name: String, value: String }

type alias Element = { tag: String, attributes: List Attribute, children: Children, value: String, id: Int }
type alias Model = { currentId: Int, element: Element }

type Children = Children (List Element)

initialModel =
  { currentId = 7, element = initialElement }

textInput id =
  let
    label = Element "label" [Attribute "for" "input1"] (Children []) "Input1" id
    inputAttrs = [Attribute "type" "text", Attribute "class" "form-control", Attribute "id" "input1"]
    input = Element "input" inputAttrs (Children []) "" id
  in
    Element "div" [Attribute "class" "form-group"] (Children [label, input]) "" id

select id =
  let
    label = Element "label" [Attribute "for" ("input" ++ (toString id))] (Children []) "Example select" id
    option1 = Element "option" [] (Children []) "1" id
    option2 = Element "option" [] (Children []) "2" id
    option3 = Element "option" [] (Children []) "3" id
    selectx = Element "select" [Attribute "class" "form-control", Attribute "id" ("input" ++ (toString id))] (Children [option1, option2, option3]) "" id
  in
    Element "div" [Attribute "class" "form-group"] (Children [label, selectx]) "" id

multiselect id =
  let
    label = Element "label" [Attribute "for" ("input" ++ (toString id))] (Children []) "Example select" id
    option1 = Element "option" [] (Children []) "1" id
    option2 = Element "option" [] (Children []) "2" id
    option3 = Element "option" [] (Children []) "3" id
    option4 = Element "option" [] (Children []) "4" id
    option5 = Element "option" [] (Children []) "5" id
    selectx = Element "select" [Attribute "class" "form-control", Attribute "id" ("input" ++ (toString id)), Attribute "multiple" ""] (Children [option1, option2, option3, option4, option5]) "" id
  in
    Element "div" [Attribute "class" "form-group"] (Children [label, selectx]) "" id

textarea id =
  let
    label = Element "label" [Attribute "for" ("input" ++ (toString id))] (Children []) "Example textarea" id
    textareax = Element "textarea" [Attribute "class" "form-control", Attribute "id" ("input" ++ (toString id)), Attribute "rows" "3"] (Children []) "" id
  in
    Element "div" [Attribute "class" "form-group"] (Children [label, textareax]) "" id

fileUpload id =
  let
    label = Element "label" [Attribute "for" ("input" ++ (toString id))] (Children []) "Example file upload" id
    upload = Element "input" [Attribute "class" "form-control-file", Attribute "id" ("input" ++ (toString id)), Attribute "type" "file", Attribute "aria-describedby" "fileHelp"] (Children []) "" id
  in
    Element "div" [Attribute "class" "form-group"] (Children [label, upload]) "" id

radioButtons id =
  let
    legend = Element "legend" [] (Children []) "Radio buttons" id

    input1 = Element "input" [Attribute "type" "radio", Attribute "class" "form-check-input", Attribute "name" ("optionsRadio" ++ (toString id)), Attribute "id" ("optionsRadio" ++ (toString id) ++ "1"), Attribute "value" "option1"] (Children []) "" id
    label1 = Element "label" [Attribute "class" "form-check-label"] (Children [input1]) "Option 1" id
    div1 = Element "div" [Attribute "class" "form-check"] (Children [label1]) "" id

    input2 = Element "input" [Attribute "type" "radio", Attribute "class" "form-check-input", Attribute "name" ("optionsRadio" ++ (toString id)), Attribute "id" ("optionsRadio" ++ (toString id) ++ "2"), Attribute "value" "option2"] (Children []) "" id
    label2 = Element "label" [Attribute "class" "form-check-label"] (Children [input2]) "Option 2" id
    div2 = Element "div" [Attribute "class" "form-check"] (Children [label2]) "" id

    input3 = Element "input" [Attribute "type" "radio", Attribute "class" "form-check-input", Attribute "name" ("optionsRadio" ++ (toString id)), Attribute "id" ("optionsRadio" ++ (toString id) ++ "3"), Attribute "value" "option3"] (Children []) "" id
    label3 = Element "label" [Attribute "class" "form-check-label"] (Children [input3]) "Option 2" id
    div3 = Element "div" [Attribute "class" "form-check"] (Children [label3]) "" id
  in
    Element "fieldset" [Attribute "class" "form-group"] (Children [legend, div1, div2, div3]) "" id

checkbox id =
  let
    input = Element "input" [Attribute "type" "checkbox", Attribute "class" "form-check-input"] (Children []) "" id
    label = Element "label" [Attribute "class" "form-check-label"] (Children [input]) "Check me out" id
  in
    Element "div" [Attribute "class" "form-check"] (Children [label]) "" id

button id =
  Element "button" [Attribute "type" "submit", Attribute "class" "btn btn-primary"] (Children []) "Submit button" id

removeElementsRecursive model id =
  let
    element = model.element
    childs = (\ (Children childs) -> childs) element.children
    removeFunc = (\child -> if child.id == id then Nothing else Just child)
  in
    { element | children = Children (List.filterMap removeFunc childs) }

-------------
-- Private --
-------------

generateNextId model =
  model.currentId + 1

initialElement =
  Element "form" [] (Children [initialTextInput, initialCheckbox, initialSubmit]) "" 0

initialTextInput =
  let
    label = Element "label" [Attribute "for" "input1"] (Children []) "Input1" 1
    inputAttrs = [Attribute "type" "text", Attribute "class" "form-control", Attribute "id" "input1"]
    input = Element "input" inputAttrs (Children []) "" 2
  in
    Element "div" [Attribute "class" "form-group"] (Children [label, input]) "" 3

initialCheckbox =
  let
    checkInput = Element "input" [Attribute "type" "checkbox", Attribute "class" "form-check-input"] (Children []) "" 4
    checkLabel = Element "label" [Attribute "class" "form-check-label"] (Children [checkInput]) "Check me out" 5
  in
    Element "div" [Attribute "class" "form-check"] (Children [checkLabel]) "" 6

initialSubmit =
  let
    submitAttrs = [Attribute "type" "submit", Attribute "class" "btn btn-primary"]
  in
    Element "button" submitAttrs (Children []) "Submit" 7

isDeletable model =
  let
    classAttr = List.head (List.filter (\attr -> attr.name == "class") model.attributes)
  in
    case classAttr of
      Nothing ->
        False
      Just a ->
        List.any (\className -> List.member className (String.split " " a.value)) ["form-group", "btn", "form-check"]
        -- List.any (\className -> String.contains className a.value) ["form-group", "btn"]
    -- True
    -- List.any (\className -> String.contains className classAttr.value) ["form-group", "btn"]
    -- List.any (\className -> String.contains className classAttr.value) ["form-group", "btn"]
    -- (List.any (\attr -> attr.name == "class") attributes)
    -- && (List.any (\attr -> String.contains attr.value == "class") attributes)
    -- List.any (\attr -> attr.value == "btn btn-primary") attributes
    -- String.contains "div" model.tag
    -- "div" == "div"
    -- model.tag == "button"
  -- True


-- isDeletable attribute =
--   -- (attribute.name == "class" && (List.any (\className -> String.contains className attribute.value) ["form-group", "btn"]))
--   -- True
--   attribute.name == "class"

isVoid element =
  List.member element.tag voidElementsList

voidElementsList =
    ["area", "base", "br", "col", "command", "embed", "hr", "img", "input", "keygen", "link", "meta", "param", "source", "track", "wbr"]
