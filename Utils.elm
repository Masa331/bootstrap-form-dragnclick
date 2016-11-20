port module Utils exposing (..)

import String
import ElementMap

compact : List String -> List String
compact list =
  List.filter (not << String.isEmpty) list

find : (a -> Bool) -> List a -> Maybe a
find findFunc list =
  List.filter findFunc list
    |> List.head

port getFormMap : String -> Cmd msg
port determinedFormMap : (ElementMap.ElementMap -> msg) -> Sub msg
