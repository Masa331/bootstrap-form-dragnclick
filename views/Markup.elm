module Markup exposing (view, inputView)

import Html exposing (..)
import Html.Attributes exposing (..)
import String

import Models exposing (..)
import Messages exposing (..)

view : Model -> Html Msg
view model =
  let
    inputs = List.map inputMarkup model.form
    inputsString = String.join "\n" inputs
  in
    pre
      []
      [ text ("<form>\n" ++ inputsString ++ "\n</form>") ]

inputView : Input -> Html Msg
inputView inp =
  pre
    []
    [ text ("<form>\n" ++ inputMarkup inp ++ "\n</form>") ]

inputMarkup : Input -> String
inputMarkup el =
  case el of
    TextInput _ ->
      textInputCode el
    TextArea _ ->
      textAreaCode el
    _ ->
      textInputCode el

textInputCode : Input -> String
textInputCode inp =
  let
    line1 = "  <div class=\"form-group\">"
    line2 = "    <label for=\"input1\">Input 1</label>"
    line3 = "    <input type=\"text\" class=\"form-control\" id=\"input1\" placeholder=\"neco\">"
    line4 = "  </div>"
  in
    String.join "\n" [line1, line2, line3, line4]


textAreaCode : Input -> String
textAreaCode inp =
  let
    line1 = "  <div class=\"form-group\">"
    line2 = "    <label for=\"input2\">Input 2</label>"
    line3 = "    <textarea class=\"form-control\" id=\"input2\" rows=3>"
    line4 = "  </div>"
  in
    String.join "\n" [line1, line2, line3, line4]
