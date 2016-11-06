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
import ElementMap exposing (..)
import HtmlTree exposing (..)
import FormModel exposing (..)

import HtmlTreeBuilder exposing (..)

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
    inputs1 = List.map HtmlTreeBuilder.buildWithControlElements model.form
    htmlTreeWithControlElements = HtmlTree.Element "form" [] (HtmlTree.Children inputs1) "" []

    inputs2 = List.map HtmlTreeBuilder.buildRaw model.form
    rawHtmlTree = HtmlTree.Element "form" [] (HtmlTree.Children inputs2) "" []
  in
    div
      []
      [ div
        [ class "row" ]
        [ div
           [ class "col-sm-12" ]
           [ div [ class "bd-example" ] ([h1 [] [text "Register form"]] ++ (Form.view htmlTreeWithControlElements))
           , draggedElement model
           -- , div [ class "highlight" ] [ Markup.view rawHtmlTree ]
           ]
        -- , div
        --    [ class "col-sm-4" ]
        --    Templates.view
        ]
      ]

inputEdit : Input -> Html Msg
inputEdit input =
  let
    inputs = [HtmlTreeBuilder.buildWithControlElements input]
    htmlTree = HtmlTree.Element "form" [] (HtmlTree.Children inputs) "" []
  in
    div
      []
      [ a [ href "javascript:void(0)", onClick (FormMessage StopEditing) ] [text "Back to form" ]
      , div
          [ class "row" ]
          [ div
             [ class "col-sm-8" ]
             [ div [ class "bd-example" ] (Form.view htmlTree)
             , div [ class "highlight" ] [ Markup.view htmlTree ] ]
          , div
             [ class "col-sm-4" ]
             (InputOptions.view input)
          ]
      ]

-------------
-- Helpers --
-------------

draggedElement model =
  case Models.currentlyDraggedInput model of
    Nothing ->
      div [] []
    Just element ->
      let
        input = HtmlTreeBuilder.buildDragged element
        htmlTree = HtmlTree.Element "form" [] (HtmlTree.Children [input]) "" []
        content = Form.view htmlTree

        dimensions =
          case dimensionsById model.elementMap (toString element.id) of
            Nothing ->
              (0, 0, 0, 0)
            Just dims ->
              ((round dims.x) + (model.mousePosition.x - model.initialMousePosition.x)
              , (round dims.y) + (model.mousePosition.y - model.initialMousePosition.y)
              , round dims.width
              , round dims.height)

        (topx, topy, widthx, heightx) = dimensions
        top = ("top", (toString topy) ++ "px")
        left = ("left", (toString topx) ++ "px")
        width = ("width", (toString widthx) ++ "px")
        height = ("height", (toString heightx) ++ "px")
        attrs = [style [("position", "fixed"), top, left, width, height]]
      in
        div attrs content
