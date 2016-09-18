module InputUpdate exposing (..)

import Messages exposing (..)
import Models exposing (..)

update msg model =
  case msg of
    RemoveInput id ->
      removeInput model id
    EditInput id ->
      ({ model | currentlyEddited = Just id }, Cmd.none)
    StopEditing ->
      ({ model | currentlyEddited = Nothing }, Cmd.none)
    PlaceholderEdit placeholder ->
      (model, Cmd.none)

-------------
-- Private --
-------------

removeInput model inputId =
  let
    newElement = removeElement model inputId
  in
    ({ model | element = newElement }, Cmd.none)




--
-- type FormMsg =
--   AddTextInput
--   | AddSelect
--
-- type InputMsg =
--   RemoveInput Int
--   | EditInput (Int)
--   | StopEditing
--   | PlaceholderEdit (String)
--
-- type Msg =
--   FormMessage FormMsg
--   | InputMessage InputMsg
--
-- -- view
--
-- div [ onInput (InputMessage PlaceholderEdit) ] [ input [class "form-control"] []
