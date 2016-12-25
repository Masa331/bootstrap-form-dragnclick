module Views exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import MarkupView exposing (view)
import FormView exposing (view)
import InputEditView exposing (..)
import Messages exposing (..)
import Models exposing (..)
import ElementMap exposing (..)
import HtmlNode exposing (..)
import Form exposing (..)
import Inputs exposing (..)

import HtmlTreeBuilder exposing (..)


view : Model -> Html Msg
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
    inputs = List.map HtmlTreeBuilder.buildWithControlElements model.inputs
    htmlTreeWithControlElements = HtmlNode.form "" [] [] inputs
  in
    Html.div
      [ class "row" ]
      [ Html.div
         [ class "col-sm-12" ]
         [ Html.div
           [ class "form-container" ]
           [ Html.div
             [ class "form-controls" ]
             [ Html.a [href "javascript:void(0);", onClick (FormMessage AddInput)] [ text "Add field" ]
             , Html.a [href "#source"] [ text "Show source code"]
             ]
           , Html.div
             [ class "inner-container" ]
             ([ h1 [] [text "The Form"] ] ++ (FormView.view htmlTreeWithControlElements) ++ [draggedElement model])
           ]
        ]
      ]

inputEdit : Input -> Html Msg
inputEdit input =
  let
    inputs = [ HtmlTreeBuilder.forInputEdit input ]
    htmlTree = HtmlNode.form "" [] [] inputs
  in
    Html.div
      [ class "row" ]
      [ Html.div
         [ class "col-sm-12" ]
         [ Html.div
           [ class "form-container form-sm" ]
           [ Html.div
               [ class "form-controls" ]
               [ Html.a [ href "#form" ] [ text "Back to form" ] ]
           , Html.div
             [ class "inner-container" ]
             ((FormView.view htmlTree) ++ [hr [] []] ++ ((InputEditView.view input)))
           ]
         ]
      ]

source : Model -> Html Msg
source model =
  let
    markup = List.map HtmlTreeBuilder.buildRaw model.inputs
    htmlRaw = HtmlNode.form "" [] [] markup
  in
    Html.div
      [ class "row" ]
      [ Html.div
         [ class "col-sm-12" ]
         [ Html.div
           [ class "form-container" ]
           [ Html.div
             [ class "form-controls" ]
             [ Html.a [href "#form"] [ text "Back to form"] ]
           , Html.div
             []
             [(MarkupView.view htmlRaw)]
           ]
         ]
      ]

-------------
-- Helpers --
-------------

draggedElement : Model -> Html Msg
draggedElement model =
  case Models.currentlyDraggedInput model of
    Nothing ->
      Html.div [] []
    Just input ->
      let
        element = HtmlTreeBuilder.buildDragged input
        htmlTree = HtmlNode.form "" [] [] [element]
        content = FormView.view htmlTree

        dimensions =
          case input.dimensions of
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
        Html.div attrs content
