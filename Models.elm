module Models exposing (Model, initialModel)

-- type alias Attributes = Children (List Model)

type alias Attribute = { name: String, value: String }

type Model = Model { tag: String, attributes: List Attribute, children: List Model }

initialModel =
  Model { tag = "input", attributes = [], children = [] }

initialModel2 =
  Model { tag = "input", attributes = [], children = [] }
