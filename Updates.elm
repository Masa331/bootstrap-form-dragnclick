module Updates exposing (..)

import Messages exposing (..)
import Models exposing (..)
import ElementMap exposing (..)
import Utils

import InputUpdate exposing (..)
import FormUpdate exposing (..)
import MouseUpdate exposing (..)

formUpdate msg model =
  FormUpdate.update msg model

inputUpdate msg model =
  InputUpdate.update msg model

mouseUpdate msg model =
  MouseUpdate.update msg model
