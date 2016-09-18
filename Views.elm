module Views exposing (..)

import Html exposing (..)

import Templates exposing (..)
import YourForm exposing (..)
import Markup exposing (..)

templates =
  Templates.view

yourForm model =
  YourForm.view model

markup model =
  Markup.view model
