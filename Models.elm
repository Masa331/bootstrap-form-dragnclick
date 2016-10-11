module Models exposing (Model, initial, currentlyEdditedInput, maxInputId)

import HtmlTree
import FormModel exposing (blankInput, textInput, textArea, select, checkbox, button)

type alias Model = { form: FormModel.Form, currentlyEdditedInputId: Maybe Int, newOption: String }

initial : Model
initial =
  let
    inputs = [ { textInput | id = 1, label = Just "Name" }
             , { textInput | id = 2, label = Just "E-mail" }
             , { textInput | id = 3, label = Just "Password" }
             , { checkbox | id = 4, label = Just "Send newsletter" }
             , { button | id = 5, label = Just "Register!" }
             ]
  in
    Model inputs Nothing ""

currentlyEdditedInput : Model -> Maybe FormModel.Input
currentlyEdditedInput model =
  case model.currentlyEdditedInputId of
    Nothing ->
      Nothing
    Just id ->
      List.head (List.filter (\el -> el.id == id) model.form)

maxInputId : Model -> Int
maxInputId model =
  Maybe.withDefault 0
  <| List.maximum
  <| List.map .id model.form
