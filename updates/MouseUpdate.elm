module MouseUpdate exposing (..)

import Messages exposing (..)
import Utils exposing (..)

update msg model =
  case msg of
    MouseClick id ->
      let
        updateFunc = (\input -> { input | dragged = True })
        updatedInputs = List.map (\inp -> if inp.id == id then updateFunc inp else inp) model.form
        newModel = { model | form = updatedInputs  }
      in
-- This coulds be easily improved to call for new map only on first down(no dragged element..)
        (newModel, Utils.getFormMap "unused_nonsense")
    MouseUp _ ->
      let
        updatedInputs = List.map (\input -> { input | dragged = False }) model.form
      in
       ({ model | form = updatedInputs
                , mousePosition = { x = 0, y = 0 }
                , initialMousePosition = { x = 0, y = 0 } }
       , Cmd.none)
    MouseMove position ->
      let
        initialPosition =
          case model.initialMousePosition.x + model.initialMousePosition.y of
            0 ->
              position
            _ ->
              model.initialMousePosition

        newModel = { model | mousePosition = position, initialMousePosition = initialPosition }
      in
        (moveInputs newModel position, Cmd.none)

moveInputs model mousePosition =
  let
    dimensions = List.concat model.elementMap
    draggedElementsIds = List.filter (\el -> el.dragged) model.form
      |> List.map (\el -> toString el.id)

    mapFunc = (\y -> if List.member y.id draggedElementsIds then { id = y.id, yMiddle = ((toFloat mousePosition.y) + (y.height / 2))} else { id = y.id, yMiddle = (y.top + (y.height / 2))})

    ysWithMove = List.map mapFunc dimensions
    sorted = List.sortBy .yMiddle ysWithMove
      |> List.map .id

    mapInputFunc = (\id -> List.filter (\input -> toString input.id == id) model.form |> List.head)
    sortedInputs = List.filterMap mapInputFunc sorted
  in
    { model | form = sortedInputs }
