-- module Models exposing (Model, initialModel)
module Models exposing (Model, initialModel2)

-- type alias Attributes = Children (List Model)

type alias Attribute = { name: String, value: String }

type Model = Model { tag: String, attributes: List Attribute, children: List Model }

initialModel =
  Model { tag = "input", attributes = [], children = [] }

initialModel2 =
  Model { tag = "form", attributes = [], children = initialChildren }

initialChildren =
  let
    label = Model { tag = "label", attributes = [{ name = "for", value = "input1" }], children = [] }
    input = Model { tag = "input", attributes = [{ name = "type", value = "text" }, { name = "class", value = "form-control" }, { name = "id", value = "input1" }], children = [] }
    wholeInput = Model { tag = "div", attributes = [{ name = "class", value = "form-group" }], children = [label, input] }

    submit = Model { tag = "button", attributes = [{ name = "type", value = "submit" }, { name = "class", value = "btn btn-primary"}], children = [Model { tag = "_rawText", attributes = [{ name = "value", value = "Submit" }], children = [] } ]}
  in
    [wholeInput, submit]
