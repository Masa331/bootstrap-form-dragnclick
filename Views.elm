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
  case model.currentlyEdditedInputId of
    Nothing ->
      formCreator model
    Just id ->
      inputEditLayout model id

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
         , div [class "highlight"] (markup model)]
      , div
         [ class "col-sm-4" ]
         templates
      , div [] [text (toString model)]
      ]
    ]

inputEditLayout model id =
  let
    -- childs = (\ (Children childs) -> childs) model.element.children
    input = List.head (List.filter (\el -> extractId el == id) model.form)
  in
    case input of
      Nothing ->
        div [] [text "Nothing to edit"]
      Just b ->
        div
          []
          [ a [href "javascript:void(0)", onClick (FormMessage StopEditing)] [text "Back to form"]
          , div
              [ class "row"]
              [ div
                 [ class "col-sm-8"]
                 [ div [class "bd-example"] (inputEdit b)
                 , div [class "highlight"] (inputMarkup b)]
              , div
                 [ class "col-sm-4"]
                 (inputOptions b)
              ]
          ]


templates =
  Templates.view

formEdit model =
  [FormEdit.view model]

markup model =
  [Markup.view model]

inputMarkup model =
  [Markup.inputView model]

inputEdit model =
  InputEdit.view model

inputOptions model =
  InputOptions.view model
