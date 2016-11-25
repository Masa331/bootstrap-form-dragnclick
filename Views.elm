module Views exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

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
  case currentPage model of
    Just location ->
      case location of
        Form ->
          formEdit model
        Source ->
          source model
        InputEdit id ->
          let
            input = List.head (List.filter (\el -> el.id == id) model.inputs)
          in
            case input of
              Nothing ->
                formEdit model
              Just input ->
                inputEdit input
    Nothing ->
      formEdit model

-------------
-- Private --
-------------

formEdit : Model -> Html Msg
formEdit model =
  let
    inputs1 = List.map HtmlTreeBuilder.buildWithControlElements model.inputs
    htmlTreeWithControlElements = HtmlTree.Element "form" [] (HtmlTree.Children inputs1) "" []
  in
    div
      []
      [ div
        [ class "row" ]
        [ div
           [ class "col-sm-12" ]
           [ div
             [ class "form-container form-sm" ]
             [ div
               [ class "form-controls" ]
               [ a [href "javascript:void(0);", onClick (FormMessage AddInput)] [ text "Add field" ]
               , a [href "#source"] [ text "Show source code"]]
             , div
               [ class "bd-example" ]
               ([h1 [] [text "The Form"]] ++ (Form.view htmlTreeWithControlElements))
             , draggedElement model
             ]
           ]
        ]
      ]

inputEdit : Input -> Html Msg
inputEdit input =
  let
    inputs = [HtmlTreeBuilder.forInputEdit input]
    htmlTree = HtmlTree.Element "form" [] (HtmlTree.Children inputs) "" []
  in
    div
      []
      [ div
        [ class "row" ]
        [ div
           [ class "col-sm-12" ]
           [ div
             [ class "form-container form-sm" ]
             [ div
                 [ class "form-controls" ]
                 [ a [ href "#form" ] [ text "Back to form" ] ]
             , div
               [ class "bd-example" ]
               ((Form.view htmlTree) ++ [hr [] []] ++ ((InputOptions.view input)))
             ]
           ]
        ]
      ]

source : Model -> Html Msg
source model =
  let
    inputs1 = List.map HtmlTreeBuilder.buildRaw model.inputs
    htmlRaw = HtmlTree.Element "form" [] (HtmlTree.Children inputs1) "" []
  in
    div
      []
      [ div
        [ class "row" ]
        [ div
           [ class "col-sm-12" ]
           [ div
             [ class "form-container form-sm" ]
             [ div
               [ class "form-controls" ]
               [ a [href "#form"] [ text "Back to form"]]
             , div
               [ class "bd-example" ]
               [(Markup.view htmlRaw)]
             ]
           ]
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
