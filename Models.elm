-- module Models exposing (Model, Children, initialModel2)
module Models exposing (..)

type alias Attribute = { name: String, value: String }

type alias Model = { tag: String, attributes: List Attribute, children: Children, value: String }
type Children = Children (List Model)

initialModel2 =
  Model "form" [] initialChildren ""

initialChildren =
  let
    label = Model "label" [Attribute "for" "input1"] (Children []) "Input1"
    inputAttrs = [Attribute "type" "text", Attribute "class" "form-control", Attribute "id" "input1", Attribute "disabled" ""]
    input = Model "input" inputAttrs (Children []) ""
    wholeInput = Model "div" [Attribute "class" "form-group"] (Children [label, input]) ""

    checkInput = Model "input" [Attribute "type" "checkbox", Attribute "class" "form-check-input"] (Children []) ""
    checkLabel = Model "label" [Attribute "class" "form-check-label"] (Children [checkInput]) "Check me out"
    wholeCheck = Model "div" [Attribute "class" "form-check"] (Children [checkLabel]) ""

    submitAttrs = [Attribute "type" "submit", Attribute "class" "btn btn-primary"]
    submit = Model "button" submitAttrs (Children []) "Submit"
  in
    Children [wholeInput, wholeCheck, submit]


isVoid model =
  List.member model.tag voidElementsList

voidElementsList =
    ["area", "base", "br", "col", "command", "embed", "hr", "img", "input", "keygen", "link", "meta", "param", "source", "track", "wbr"]
