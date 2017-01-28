module MouseUpdate exposing (..)

import Messages exposing (..)
import Models exposing (..)
import Utils exposing (..)
import Form exposing (..)

update : MouseMsg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MouseClick id ->
      let
        updateFunc = (\input -> { input | dragged = True })
        updatedInputs = Form.mapInputs (\inp -> if inp.id == id then updateFunc inp else inp) model.inputs
        newModel = { model | inputs = updatedInputs }
      in
-- This coulds be easily improved to call for new map only on first down(no dragged element..)
        (newModel, Utils.getFormMap "unused_nonsense")
    MouseUp _ ->
      let
        updatedInputs = Form.mapInputs (\input -> { input | dragged = False }) model.inputs
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
      -- List.map (\row -> (List.maximum (List.map (\input -> countYMiddle input model) row), row)) model.inputs
      List.map (\row -> (countRowYMiddle row model, row)) model.inputs
      |> List.sortBy Tuple.first
      |> List.map Tuple.second
  in
    { model | inputs = sortedInputs }

countRowYMiddle row model =
  List.map (\input -> countYMiddle input model) row
    |> List.maximum
    |> Maybe.withDefault 0

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
