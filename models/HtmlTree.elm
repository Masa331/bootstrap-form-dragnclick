module HtmlTree exposing (..)

type alias Attribute = { name: String, value: String }
type Children = Children (List Element)
type alias Element = { tag: String, attributes: List Attribute, children: Children, value: String }

removeElementRecursively : String -> Element -> Element
removeElementRecursively tag tree =
  let
    childs = (\ (Children childs) -> childs) tree.children
    filteredChilds = (List.filter (\child -> child.tag /= tag) childs)
  in
    case childs of
      [] ->
        Element tree.tag tree.attributes (Children filteredChilds) tree.value
      x::xs ->
        let
          innerChilds = (Children (List.map (\child -> removeElementRecursively tag child) filteredChilds))
        in
          Element tree.tag tree.attributes innerChilds tree.value
