module HtmlTree exposing (..)

type alias Attribute = { name: String, value: String }
type Children = Children (List Element)
type alias Element = { tag: String, attributes: List Attribute, children: Children, value: String }
