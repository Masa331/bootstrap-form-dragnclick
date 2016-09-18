module Views exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Templates exposing (..)
import FormEdit exposing (..)
import Markup exposing (..)
import InputEdit exposing (..)
import InputOptions exposing (..)
import Messages exposing (..)
import Models exposing (..)

view model =
  case model.currentlyEddited of
    Nothing ->
      formCreator model
    Just id ->
      inputEditLayout model

-------------
-- Private --
-------------

formCreator model =
  div
    []
    [ div
      [ class "row"]
      [ div
         [ class "col-sm-8"]
         [ div [class "bd-example"] (formEdit model)
         , div [class "highlight"] (markup model.element)]
      , div
         [ class "col-sm-4"]
         templates
      ]
    ]

inputEditLayout model =
  let
    childs = (\ (Children childs) -> childs) model.element.children
    input = List.head (List.filter (\el -> (Just el.id) == model.currentlyEddited) childs)
  in
    case input of
      Nothing ->
        div [] [text "Nothing to edit"]
      Just b ->
        div
          []
          [ a [href "javascript:void(0)", onClick (InputMessage StopEditing)] [text "Back to form"]
          , div
              [ class "row"]
              [ div
                 [ class "col-sm-8"]
                 [ div [class "bd-example"] (inputEdit model)
                 , div [class "highlight"] (markup b)]
              , div
                 [ class "col-sm-4"]
                 (inputOptions model)
              ]
          ]

templates =
  Templates.view

formEdit model =
  FormEdit.view model

markup model =
  Markup.view model

inputEdit model =
  InputEdit.view model

inputOptions model =
  InputOptions.view model
