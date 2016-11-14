module HtmlTreeBuilder exposing (buildRaw, buildWithControlElements, buildDragged, forInputEdit)

import WithControlElements
import Raw
import Dragged
import ForInputEdit

buildWithControlElements input =
  WithControlElements.build input

buildRaw input =
  Raw.build input

buildDragged input =
  Dragged.build input

forInputEdit input =
  ForInputEdit.build input
