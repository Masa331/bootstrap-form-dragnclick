module HtmlTreeBuilder exposing (buildRaw, buildWithControlElements, buildDragged)

import WithControlElements
import Raw
import Dragged

buildWithControlElements input =
  WithControlElements.build input

buildRaw input =
  Raw.build input

buildDragged input =
  Dragged.build input
