-- module Models exposing (Model, initialModel)
module Models exposing (Model, initialModel2)

type alias Attribute = { name: String, value: String }

type alias Model = { tag: String, attributes: List Attribute, children: Children, value: String }
type Children = Children (List Model)

-- initialModel2 =
--   { tag = "input", attributes = [], children = [] }

initialModel2 =
  { tag = "form", attributes = [], children = initialChildren }

initialChildren =
  let
    label = { tag = "label", attributes = [Attribute "for" "input1"], children = [], value = "" }
    input = { tag = "input", attributes = [Attribute "type" "text", Attribute "class" "form-control", Attribute "id" "input1"], children = [], value = "" }
    wholeInput = { tag = "div", attributes = [Attribute "class" "form-group"], children = [label, input], value = "" }

    submit = { tag = "button", attributes = [Attribute "type" "submit", Attribute "class" "btn btn-primary"], children = [], value = "Submit"}
  in
    [wholeInput, submit]
