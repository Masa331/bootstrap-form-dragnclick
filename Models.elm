module Models exposing (Model, initial, currentlyEdditedInput, maxInputId)

import HtmlTree
import FormModel

type alias Model = { form: FormModel.Form, currentlyEdditedInputId: Maybe Int, newOption: String }

initial : Model
initial =
  let
    textInput = FormModel.textInput 1
    textArea = FormModel.textArea 2
    checkbox = FormModel.checkbox 3
    select1 = FormModel.select 4
    button = FormModel.button 5
  in
    Model [ textInput, textArea, select1, checkbox, button ] Nothing ""

currentlyEdditedInput : Model -> Maybe FormModel.Input
currentlyEdditedInput model =
  case model.currentlyEdditedInputId of
    Nothing ->
      Nothing
    Just id ->
      List.head (List.filter (\el -> FormModel.extractId el == id) model.form)

maxInputId : Model -> Int
maxInputId model =
  Maybe.withDefault 0
  <| List.maximum
  <| List.map FormModel.extractId model.form
