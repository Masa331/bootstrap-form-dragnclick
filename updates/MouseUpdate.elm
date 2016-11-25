module MouseUpdate exposing (..)

import Messages exposing (..)
import Utils exposing (..)

update msg model =
  case msg of
    MouseClick id ->
      let
        updateFunc = (\input -> { input | dragged = True })
        updatedInputs = List.map (\inp -> if inp.id == id then updateFunc inp else inp) model.inputs
        newModel = { model | inputs = updatedInputs  }
      in
-- This coulds be easily improved to call for new map only on first down(no dragged element..)
        (newModel, Utils.getFormMap "unused_nonsense")
    MouseUp _ ->
      let
        updatedInputs = List.map (\input -> { input | dragged = False }) model.inputs
      in
       ({ model | inputs = updatedInputs
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
        (moveInputs newModel, Cmd.none)

moveInputs model =
  let
    draggedElementsIds =
      List.filter .dragged model.inputs
      |> List.map .id
      |> List.map toString

    mapFunc =
      (\y -> if List.member y.id draggedElementsIds
                then { id = y.id, yMiddle = (y.top + y.height / 2 + ((toFloat model.mousePosition.y) - (toFloat model.initialMousePosition.y))) }
                else { id = y.id, yMiddle = (y.top + y.height / 2) })

    sortedInputs =
      List.concat model.elementMap
      |> List.map mapFunc
      |> List.sortBy .yMiddle
      |> List.map .id
      |> List.map (\id -> Utils.find (\input -> id == toString input.id ) model.inputs)
      |> List.filterMap identity
  in
    { model | inputs = sortedInputs }
