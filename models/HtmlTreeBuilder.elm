module HtmlTreeBuilder exposing (buildRaw, buildWithControlElements, buildDragged, forInputEdit)

import WithControlElements
import Raw
import Dragged
import ForInputEdit

buildWithControlElements row =
  WithControlElements.build row

buildRaw row =
  Raw.build row

buildDragged input =
  Dragged.build input

forInputEdit input =
  ForInputEdit.build input
