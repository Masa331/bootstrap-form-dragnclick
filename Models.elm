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
  { inputs = [ Form.SingleInput { textInput | id = 1, label = Just "Name", placeholder = Just "Max Rockatansky" }
             , Form.SingleInput { textInput | id = 3, label = Just "Email address", placeholder = Just "rockatansky@wastelands.com", addon1 = Just "@" }
             , Form.SingleInput { textInput | id = 2, label = Just "Job title", small = Just "Please tell us what do you do for living for statistical purposes." }
             , Form.SingleInput { textInput | id = 4, label = Just "Password", type_ = Inputs.Password }
             , Form.SingleInput { checkbox | id = 5, label = Just "I Accept all terms and agreements" }
             , Form.SingleInput { button | id = 6, label = Just "Register!" }
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
  <| List.map flattenInputs model.inputs

flattenInputs : Form.Row -> List Inputs.Input
flattenInputs rowOrInput =
  case rowOrInput of
    Form.GroupOfInputs a ->
      a
    Form.SingleInput b ->
      [b]

currentlyDraggedInput : Model -> Maybe Inputs.Input
currentlyDraggedInput model =
  List.map flattenInputs model.inputs
    |> List.concat
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
