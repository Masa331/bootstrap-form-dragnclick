-- module Models exposing (Model, Children, initialModel2)
module Models exposing (..)

type alias Attribute = { name: String, value: String }

type alias Element = { tag: String, attributes: List Attribute, children: Children, value: String, id: Int }
type alias Model = { currentId: Int, element: Element }

type Children = Children (List Element)

initialModel =
  { currentId = 6, element = initialElement }

textInput =
  []

-------------
-- Private --
-------------

initialElement =
  Element "form" [] initialChildren "" 0

initialChildren =
  let
    label = Element "label" [Attribute "for" "input1"] (Children []) "Input1" 1
    inputAttrs = [Attribute "type" "text", Attribute "class" "form-control", Attribute "id" "input1", Attribute "disabled" ""]
    input = Element "input" inputAttrs (Children []) "" 2
    wholeInput = Element "div" [Attribute "class" "form-group"] (Children [label, input]) "" 3

    checkInput = Element "input" [Attribute "type" "checkbox", Attribute "class" "form-check-input"] (Children []) "" 4
    checkLabel = Element "label" [Attribute "class" "form-check-label"] (Children [checkInput]) "Check me out" 5
    wholeCheck = Element "div" [Attribute "class" "form-check"] (Children [checkLabel]) "" 6

    submitAttrs = [Attribute "type" "submit", Attribute "class" "btn btn-primary"]
    submit = Element "button" submitAttrs (Children []) "Submit" 7
  in
    Children [wholeInput, wholeCheck, submit]


isVoid element =
  List.member element.tag voidElementsList

voidElementsList =
    ["area", "base", "br", "col", "command", "embed", "hr", "img", "input", "keygen", "link", "meta", "param", "source", "track", "wbr"]
