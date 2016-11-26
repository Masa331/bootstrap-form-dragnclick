module Updates exposing (..)

import InputUpdate exposing (..)
import FormUpdate exposing (..)
import MouseUpdate exposing (..)

import Messages

update msg model =
  case msg of
    Messages.InputMessage inputMsg ->
      InputUpdate.update inputMsg model
    Messages.FormMessage formMsg ->
      FormUpdate.update formMsg model
    Messages.MouseMessage mouseMsg ->
      MouseUpdate.update mouseMsg model
    Messages.MapDetermined map ->
      (updateInputsDimensions model map, Cmd.none)
    Messages.UrlChange location ->
      ({ model | history = location :: model.history }, Cmd.none)
    Messages.NewOptionEdit value ->
      ({ model | newOption = value }, Cmd.none)

-------------
-- Private --
-------------

updateInputsDimensions model map =
  let
    flatMap = List.concat map
    updateFunction = (\input -> { input | dimensions = (List.filter (\e -> e.id == toString input.id) flatMap) |> List.head })

    inputsWithUpdatedDimesnions =
      List.map updateFunction model.inputs
  in
    { model | inputs = inputsWithUpdatedDimesnions }
