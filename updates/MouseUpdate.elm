module MouseUpdate exposing (..)

import Messages exposing (..)
import Models exposing (..)
import Utils exposing (..)

update : MouseMsg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MouseClick id ->
      let
        updateFunc = (\input -> { input | dragged = True })
        updatedInputs = List.map (\inp -> if inp.id == id then updateFunc inp else inp) model.inputs
        newModel = { model | inputs = updatedInputs }
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
    sortedInputs =
      List.map (\input -> (countYMiddle input model, input)) model.inputs
      |> List.sortBy Tuple.first
      |> List.map Tuple.second
  in
    { model | inputs = sortedInputs }

countYMiddle input model =
  case input.dimensions of
    Nothing ->
      0
    Just dimensions ->
      if input.dragged
        then
          (dimensions.top + dimensions.height / 2 + ((toFloat model.mousePosition.y) - (toFloat model.initialMousePosition.y)))
        else
          (dimensions.top + dimensions.height / 2)



