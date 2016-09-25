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
         [ div [class "bd-example"] [FormEdit.view model]
         , div [class "highlight"] [Markup.view model]]
      , div
         [ class "col-sm-4" ]
         Templates.view
      ]
    ]

inputEditLayout model id =
  let
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
                 [ div [class "bd-example"] [InputEdit.view b]
                 , div [class "highlight"] [Markup.inputView b]]
              , div
                 [ class "col-sm-4"]
                 (InputOptions.view b)
              ]
          ]
