port module Main exposing (..)

-- import Html exposing (..)
import Html
import Html.App

initialModel = Html.text "boo"

init = (initialModel, decodeElm initialModel)

view model =
  text "Ahoj svete!"

update msg model =
  (model, Cmd.none)

subscriptions model =
  Sub.none

port decodeElm : VirtualDom.Node -> Cmd msg

main =
  Html.App.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
