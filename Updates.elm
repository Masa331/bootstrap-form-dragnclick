module Updates exposing (..)

import Messages exposing (..)
import Models exposing (..)

import InputUpdate exposing (..)
import FormUpdate exposing (..)

formUpdate msg model =
  FormUpdate.update msg model

inputUpdate msg model =
  InputUpdate.update msg model