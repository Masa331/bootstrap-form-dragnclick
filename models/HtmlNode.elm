module HtmlNode exposing (..)

import Html.Events
import Html

import Messages

type alias Attribute = { name: String, value: String }
type alias NodeAttrs = { tag: String
                       , value: String
                       , attributes: List Attribute
                       , events: List (Html.Attribute Messages.Msg)
                       , children: List Node
                       }
type Node = Node NodeAttrs

div value attributes events children =
  Node { tag = "div", value = value, attributes = attributes, events = events, children = children }

button value attributes events children =
  Node { tag = "button", value = value, attributes = attributes, events = events, children = children }

span value attributes events children =
  Node { tag = "span", value = value, attributes = attributes, events = events, children = children }

i value attributes events children =
  Node { tag = "i", value = value, attributes = attributes, events = events, children = children }

a value attributes events children =
  Node { tag = "a", value = value, attributes = attributes, events = events, children = children }

input value attributes events children =
  Node { tag = "input", value = value, attributes = attributes, events = events, children = children }

label value attributes events children =
  Node { tag = "label", value = value, attributes = attributes, events = events, children = children }

fieldset value attributes events children =
  Node { tag = "fieldset", value = value, attributes = attributes, events = events, children = children }

small value attributes events children =
  Node { tag = "small", value = value, attributes = attributes, events = events, children = children }

legend value attributes events children =
  Node { tag = "legend", value = value, attributes = attributes, events = events, children = children }

select value attributes events children =
  Node { tag = "select", value = value, attributes = attributes, events = events, children = children }

option value attributes events children =
  Node { tag = "option", value = value, attributes = attributes, events = events, children = children }

textarea value attributes events children =
  Node { tag = "textarea", value = value, attributes = attributes, events = events, children = children }

form value attributes events children =
  Node { tag = "form", value = value, attributes = attributes, events = events, children = children }
