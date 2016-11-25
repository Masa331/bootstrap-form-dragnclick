module Models exposing (..)

import Mouse

import HtmlTree
import ElementMap
import Navigation
import UrlParser exposing ((</>))
import FormModel exposing (textInput, checkbox, button)

type Route = Form
           | Source
           | InputEdit Int

type alias Model = { inputs: FormModel.Form
                   , newOption: String
                   , mousePosition : Mouse.Position
                   , initialMousePosition : Mouse.Position
                   , elementMap : ElementMap.ElementMap
                   , history : List Navigation.Location
                   }

initial : Model
initial =
  { inputs = [ { textInput | id = 1, label = Just "Name", placeholder = Just "Max Rockatansky" }
             , { textInput | id = 2, label = Just "Job title", placeholder = Just "Sheep herder", small = Just "Please tell us what do you do for living for statistical purposes." }
             , { textInput | id = 3, label = Just "Email address", placeholder = Just "rockatansky@wastelands.com", addon1 = Just "@" }
             , { textInput | id = 4, label = Just "Password", type_ = FormModel.Password }
             , { checkbox | id = 5, label = Just "I Accept all terms and agreements" }
             , { button | id = 6, label = Just "Register!" }
             ]
  , newOption = ""
  , mousePosition = { x = 0, y = 0 }
  , initialMousePosition = { x = 0, y = 0 }
  , elementMap = [[]]
  , history = []
  }

maxInputId : Model -> Int
maxInputId model =
  Maybe.withDefault 0
  <| List.maximum
  <| List.map .id model.inputs

currentlyDraggedInput model =
  List.filter .dragged model.inputs
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
