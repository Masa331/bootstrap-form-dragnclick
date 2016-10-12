module Utils exposing (compact)

import String

compact : List String -> List String
compact list =
  List.filter (not << String.isEmpty) list
