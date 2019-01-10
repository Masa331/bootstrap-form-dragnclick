module Models exposing (..)

import Mouse

import HtmlNode
import ElementMap
import Navigation
import UrlParser exposing ((</>))
import Inputs exposing (Input, textInput, checkbox, button)
import Form

type Route = Form
           | Source
           | InputEdit Int

type alias Model = { inputs: Form.Form
                   , newOption: String
                   , mousePosition : Mouse.Position
                   , initialMousePosition : Mouse.Position
                   , history : List Navigation.Location
                   }

initial : Model
initial =
  { inputs = [ [ { textInput | id = 1, label = Just "Name", placeholder = Just "Max Rockatansky" }
               , { textInput | id = 1, label = Just "Name", placeholder = Just "Max Rockatansky" } ]
             , [ { textInput | id = 3, label = Just "Email address", placeholder = Just "rockatansky@wastelands.com", addon1 = Just "@" } ]
             , [ { textInput | id = 2, label = Just "Job title", small = Just "Please tell us what do you do for living for statistical purposes." } ]
             , [ { textInput | id = 4, label = Just "Password", type_ = Inputs.Password } ]
             , [ { checkbox | id = 5, label = Just "I Accept all terms and agreements" } ]
             , [ { button | id = 6, label = Just "Register!" } ]
             ]
  , newOption = ""
  , mousePosition = { x = 0, y = 0 }
  , initialMousePosition = { x = 0, y = 0 }
  , history = []
  }

maxInputId : Model -> Int
maxInputId model =
  Maybe.withDefault 0
  <| List.maximum
  <| List.map .id
  <| List.concat
  <| model.inputs

currentlyDraggedInput : Model -> Maybe Inputs.Input
currentlyDraggedInput model =
  List.concat model.inputs
    |> List.filter .dragged
    |> List.head

route : UrlParser.Parser (Route -> a) a
route =
  UrlParser.oneOf
   [ UrlParser.map Form (UrlParser.s "form")
   , UrlParser.map Source (UrlParser.s "source")
   , UrlParser.map InputEdit (UrlParser.s "input" </> UrlParser.int)
   ]

currentPage : Model -> Maybe Route
currentPage model =
  case List.head model.history of
    Just location ->
      UrlParser.parseHash route location
    Nothing ->
      Nothing
