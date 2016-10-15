port module Utils exposing (..)

import String
import ElementMap

compact : List String -> List String
compact list =
  List.filter (not << String.isEmpty) list

port getFormMap : String -> Cmd msg
port determinedFormMap : (ElementMap.ElementMap -> msg) -> Sub msg
