module Models exposing (Model, initial, currentlyEdditedInput, maxInputId, currentlyDraggedInputs, currentlyDraggedInput)

import Mouse

import HtmlTree
import ElementMap
import FormModel exposing (blankInput, textInput, textArea, select, checkbox, button)

type alias Model = { form: FormModel.Form
                   , currentlyEdditedInputId: Maybe Int
                   , newOption: String
                   , mousePosition : Mouse.Position
                   , initialMousePosition : Mouse.Position
                   , elementMap : ElementMap.ElementMap
                   }

initial : Model
initial =
  let
    inputs = [ { textInput | id = 1, label = Just "Name", placeholder = Just "Max Rockatansky", addon1 = Just "$" }
             , { textInput | id = 2, label = Just "Job title", placeholder = Just "Sheep herder", small = Just "Please tell us what do you do for living for statistical purposes." }
             , { textInput | id = 3, label = Just "Email address", placeholder = Just "rockatansky@wastelands.com" }
             , { textInput | id = 4, label = Just "Password", type' = FormModel.Password }
             , { checkbox | id = 5, label = Just "I Accept all terms and agreements" }
             , { button | id = 6, label = Just "Register!" }
             ]
  in
    Model inputs Nothing "" { x = 0, y = 0 } { x = 0, y = 0 } [[]]

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

currentlyDraggedInputs model =
  List.filter .dragged model.form

currentlyDraggedInput model =
  List.filter .dragged model.form
    |> List.head
