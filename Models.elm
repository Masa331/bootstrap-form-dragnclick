-- module Models exposing (Model, Children, initialModel2)
module Models exposing (..)

type alias Attribute = { name: String, value: String }

type alias Element = { tag: String, attributes: List Attribute, children: Children, value: String, id: Int }
type alias Model = { currentId: Int, element: Element }

type Children = Children (List Element)

initialModel =
  { currentId = 7, element = initialElement }

textInput id =
  let
    label = Element "label" [Attribute "for" "input1"] (Children []) "Input1" id
    inputAttrs = [Attribute "type" "text", Attribute "class" "form-control", Attribute "id" "input1", Attribute "disabled" ""]
    input = Element "input" inputAttrs (Children []) "" id
  in
    Element "div" [Attribute "class" "form-group"] (Children [label, input]) "" id

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
    inputAttrs = [Attribute "type" "text", Attribute "class" "form-control", Attribute "id" "input1", Attribute "disabled" ""]
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

isVoid element =
  List.member element.tag voidElementsList

voidElementsList =
    ["area", "base", "br", "col", "command", "embed", "hr", "img", "input", "keygen", "link", "meta", "param", "source", "track", "wbr"]
