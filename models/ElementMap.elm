module ElementMap exposing (..)

type alias ElementDimensions =
  { x : Float
  , y : Float
  , width: Float
  , height: Float
  , top: Float
  , right: Float
  , bottom: Float
  , left: Float
  }
type alias ElementMap
  = List (List ElementDimensions)
