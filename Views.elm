module Views exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Templates exposing (..)
import Markup exposing (view)
import Form exposing (view)
import InputOptions exposing (..)
import Messages exposing (..)
import Models exposing (..)
import HtmlTree exposing (..)
import FormModel exposing (..)

import InputToHtmlTreeConverter exposing (..)

view model =
  case currentlyEdditedInput model of
    Nothing ->
      formEdit model
    Just input ->
      inputEdit input

-------------
-- Private --
-------------

formEdit : Model -> Html Msg
formEdit model =
  let
    inputs = List.map inputToHtmlTree model.form
    htmlTree = HtmlTree.Element "form" [] (HtmlTree.Children inputs) ""
  in
    div
      []
      [ div
        [ class "row" ]
        [ div
           [ class "col-sm-8"]
           [ div [ class "bd-example" ] (Form.view htmlTree)
           , div [ class "highlight" ] [ Markup.view htmlTree] ]
        , div
           [ class "col-sm-4" ]
           Templates.view
        ]
      ]

inputEdit : Input -> Html Msg
inputEdit input =
  let
    inputs = [inputToHtmlTree input]
    htmlTree = HtmlTree.Element "form" [] (HtmlTree.Children inputs) ""
  in
    div
      []
      [ a [href "javascript:void(0)", onClick (FormMessage StopEditing)] [text "Back to form"]
      , div
          [ class "row"]
          [ div
             [ class "col-sm-8"]
             [ div [ class "bd-example" ] (Form.view htmlTree)
             , div [ class "highlight" ] [ Markup.view htmlTree ] ]
          , div
             [ class "col-sm-4" ]
             (InputOptions.view input)
          ]
      ]
