module FormEdit exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (..)

import Models exposing (..)
import Messages exposing (..)

import Inputs exposing (..)

view : Model -> Html Msg
view model =
  let
    elements = List.map inputHtml model.form
  in
    Html.form [] elements
