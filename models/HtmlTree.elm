module HtmlTree exposing (..)

import Html.Events
import Html

import Messages

type alias Attribute = { name: String, value: String }

type Children = Children (List Element)
-- value should be refactored to Maybe String
type alias Element = { tag: String, attributes: List Attribute, children: Children, value: String, events: List (Html.Attribute Messages.Msg) }
