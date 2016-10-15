module Updates exposing (..)

import Messages exposing (..)
import Models exposing (..)
import Utils

import InputUpdate exposing (..)
import FormUpdate exposing (..)

formUpdate msg model =
  FormUpdate.update msg model

inputUpdate msg model =
  InputUpdate.update msg model

mouseUpdate msg model =
  case msg of
    MouseDown id ->
      let
        newModel = { model | currentlyDraggedInputId = Just id }
      in
-- This coulds be easily improved to call for new map only on first down(no currentlyDraggedInputId set..)
        (newModel, Utils.getFormMap "ahoj")
    MouseUp _ ->
      let
        newModel = { model | currentlyDraggedInputId = Nothing, mousePosition = { x = 0, y = 0 } }
      in
        (newModel, Cmd.none)
    MouseMove position ->
      let
        newModel = { model | mousePosition = position }
      in
        (newModel, Cmd.none)
