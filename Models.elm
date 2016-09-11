-- module Models exposing (Model, Children, initialModel2)
module Models exposing (..)

type alias Attribute = { name: String, value: String }

type alias Model = { tag: String, attributes: List Attribute, children: Children, value: String }
type Children = Children (List Model)

initialModel2 =
  Model "form" [] initialChildren ""
  -- { tag = "form", attributes = [], children = initialChildren, value = "" }
  -- { tag = "form", attributes = [], children = Children initialChildren, value = "" }
  -- Model "form" [] (Children initialChildren) ""
  -- Model "form" [] initialChildren ""

initialChildren =
  let
    label = Model "label" [Attribute "for" "input1"] (Children []) ""
    inputAttrs = [Attribute "type" "text", Attribute "class" "form-control", Attribute "id" "input1"]
    input = Model "input" inputAttrs (Children []) ""
    wholeInput = Model "div" [Attribute "class" "form-group"] (Children [label, input]) ""

    submitAttrs = [Attribute "type" "submit", Attribute "class" "btn btn-primary"]
    submit = Model "button" submitAttrs (Children []) "Submit"
  in
    Children [wholeInput, submit]
