module Views exposing (..)

import Html exposing (..)

import TemplatesView exposing (..)
import YourForm exposing (..)
import Markup exposing (..)

templatesView =
  TemplatesView.view

yourForm model =
  YourForm.view model

markup model =
  Markup.view model
