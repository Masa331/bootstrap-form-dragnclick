module Models exposing (..)

import Mouse

import HtmlTree
import ElementMap
import Navigation
import UrlParser exposing ((</>))
import FormModel exposing (blankInput, textInput, textArea, select, checkbox, button)

type Route = Form
           | Source
           | InputEdit Int

type alias Model = { form: FormModel.Form
                   , newOption: String
                   , mousePosition : Mouse.Position
                   , initialMousePosition : Mouse.Position
                   , elementMap : ElementMap.ElementMap
                   , history : List Navigation.Location
                   }

initial : Model
initial =
  let
    inputs = [ { textInput | id = 1, label = Just "Name", placeholder = Just "Max Rockatansky" }
             , { textInput | id = 2, label = Just "Job title", placeholder = Just "Sheep herder", small = Just "Please tell us what do you do for living for statistical purposes." }
             , { textInput | id = 3, label = Just "Email address", placeholder = Just "rockatansky@wastelands.com", addon1 = Just "@" }
             , { textInput | id = 4, label = Just "Password", type_ = FormModel.Password }
             , { checkbox | id = 5, label = Just "I Accept all terms and agreements" }
             , { button | id = 6, label = Just "Register!" }
             ]
  in
    Model inputs "" { x = 0, y = 0 } { x = 0, y = 0 } [[]] []

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

route =
  UrlParser.oneOf
   [ UrlParser.map Form (UrlParser.s "form")
   , UrlParser.map Source (UrlParser.s "source")
   , UrlParser.map InputEdit (UrlParser.s "input" </> UrlParser.int)
   ]

currentPage model =
  case List.head model.history of
    Just location ->
      UrlParser.parseHash route (Debug.log "location" location)
    Nothing ->
      Nothing
