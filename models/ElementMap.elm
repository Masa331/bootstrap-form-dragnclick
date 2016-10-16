module ElementMap exposing (..)

type alias ElementDimensions =
  { id: String
  , x: Float
  , y: Float
  , width: Float
  , height: Float
  , top: Float
  , right: Float
  , bottom: Float
  , left: Float
  }
type alias ElementMap
  = List (List ElementDimensions)

dimensionsById dimensions id =
  List.concat dimensions
    |> List.filter (\a -> a.id == id)
    |> List.head
