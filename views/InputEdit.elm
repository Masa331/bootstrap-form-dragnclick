module InputEdit exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (..)

import Models exposing (..)
import Messages exposing (..)

import Inputs exposing (..)

view : Input -> Html Msg
view inp =
  Html.form [] (toElmHtmlNode (inputToHtmlTree inp))
