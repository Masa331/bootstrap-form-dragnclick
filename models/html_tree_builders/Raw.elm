module Raw exposing (build)

import String
import Html.Events
import Html.Attributes

import HtmlNode exposing (..)
import Form exposing (..)
import Inputs exposing (..)
import Messages
import Models

build : Input -> HtmlNode.Node
build input =
  case input.type_ of
    Text -> textInputToHtmlTree input
    TextArea -> textAreaToHtmlTree input
    Select -> selectToHtmlTree input
    Multiselect -> multiselectToHtmlTree input
    FileUpload -> fileUploadToHtmlTree input
    Radio -> radioToHtmlTree input
    Checkbox -> checkboxToHtmlTree input
    Button -> buttonToHtmlTree input
    Color -> colorToHtmlTree input
    Password -> textInputToHtmlTree input
    Number -> textInputToHtmlTree input
    DatetimeLocal -> textInputToHtmlTree input
    Date -> textInputToHtmlTree input
    Time -> textInputToHtmlTree input

-------------
-- Private --
-------------

textInputToHtmlTree input =
  let
    inputAttrs =
      [ Just (Attribute "id" (toString input.id))
      , Maybe.map (Attribute "placeholder") input.placeholder
      , if input.disabled then Just (Attribute "disabled" "disabled") else Nothing
      , Just (Attribute "class" (String.trim ((sizeClass input.size) ++ " form-control")))
      , Just (Attribute "type" (inputTypeToString input.type_))
      ] |> List.filterMap identity

    add1 = toAddon input.addon1
    add2 = toAddon input.addon2

    input1 = Just (HtmlNode.input "" inputAttrs [] [])

    inputWithAddons =
      if List.isEmpty (List.filterMap identity [add1, add2]) then
        input1
      else
        Just (div "" [Attribute "class" "input-group"] [] ([add1, input1, add2] |> List.filterMap identity))

    children =
      [ Maybe.map (\value -> label value [Attribute "for" (toString input.id)] [] []) input.label
      , inputWithAddons
      , Maybe.map (\value -> small value [Attribute "class" "form-text text-muted"] [] []) input.small
      ] |> List.filterMap identity
  in
    div "" [Attribute "class" "form-group"] [] children

colorToHtmlTree input =
  let
    inputAttrs =
      [ Just (Attribute "id" ("input" ++ toString input.id))
      , Maybe.map (Attribute "placeholder") input.placeholder
      , if input.disabled then Just (Attribute "disabled" "disabled") else Nothing
      , Just (Attribute "class" (String.trim ((sizeClass input.size) ++ " form-control")))
      , Just (Attribute "type" (inputTypeToString input.type_))
      ] |> List.filterMap identity

    children =
      [ Maybe.map (\value -> label value [Attribute "for" "input1"] [] []) input.label
      , Just (HtmlNode.input "" inputAttrs [] [])
      , Maybe.map (\value -> small value [Attribute "class" "form-text text-muted"] [] []) input.small
      ] |> List.filterMap identity
  in
    div "" [Attribute "class" "form-group"] [] children

selectToHtmlTree input =
  let
    inputAttrs =
      [ Just (Attribute "id" ("input" ++ toString input.id))
      , if input.disabled then Just (Attribute "disabled" "disabled") else Nothing
      , Just (Attribute "class" (String.trim ((sizeClass input.size) ++ " form-control")))
      ] |> List.filterMap identity

    options = List.map (\value -> option value [] [] []) input.options
    children =
      [ Maybe.map (\value -> label value [Attribute "for" "input1"] [] []) input.label
      , Just (select "" inputAttrs [] options)
      , Maybe.map (\value -> small value [Attribute "class" "form-text text-muted"] [] []) input.small
      ] |> List.filterMap identity
  in
    div "" [Attribute "class" "form-group"] [] children

textAreaToHtmlTree input =
  let
    inputAttrs =
      [ Just (Attribute "id" ("input" ++ toString input.id))
      , Maybe.map (Attribute "placeholder") input.placeholder
      , if input.disabled then Just (Attribute "disabled" "disabled") else Nothing
      , Just (Attribute "class" (String.trim ((sizeClass input.size) ++ " form-control")))
      , Just (Attribute "rows" (input.rowNumber))
      ] |> List.filterMap identity

    add1 = toAddon input.addon1
    add2 = toAddon input.addon2
    input1 = Just (textarea "" inputAttrs [] [])
    inputWithAddons =
      if List.isEmpty (List.filterMap identity [add1, add2]) then
        input1
      else
        Just (div "" [Attribute "class" "input-group"] [] ([add1, input1, add2] |> List.filterMap identity))

    children =
      [ Maybe.map (\value -> label value [Attribute "for" "input1"] [] []) input.label
      , inputWithAddons
      , Maybe.map (\value -> small value [Attribute "class" "form-text text-muted"] [] []) input.small
      ] |> List.filterMap identity
  in
    div "" [Attribute "class" "form-group"] [] children

multiselectToHtmlTree input =
  let
    inputAttrs =
      [ Just (Attribute "id" ("input" ++ toString input.id))
      , if input.disabled then Just (Attribute "disabled" "disabled") else Nothing
      , Just (Attribute "class" (String.trim ((sizeClass input.size) ++ " form-control")))
      , Just (Attribute "multiple" "multiple")
      ] |> List.filterMap identity

    options = List.map (\value -> option value [] [] []) input.options
    children =
      [ Maybe.map (\value -> label value [Attribute "for" "input1"] [] []) input.label
      , Just (select "" inputAttrs [] options)
      , Maybe.map (\value -> small value [Attribute "class" "form-text text-muted"] [] []) input.small
      ] |> List.filterMap identity
  in
    div "" [Attribute "class" "form-group"] [] children

fileUploadToHtmlTree input =
  let
    inputAttrs =
      [ Just (Attribute "id" ("input" ++ toString input.id))
      , if input.disabled then Just (Attribute "disabled" "disabled") else Nothing
      , Just (Attribute "class" (String.trim ((sizeClass input.size) ++ " form-control-file")))
      , Just (Attribute "type" "file")
      ] |> List.filterMap identity

    children =
      [ Maybe.map (\value -> label value [Attribute "for" "input1"] [] []) input.label
      , Just (HtmlNode.input "" inputAttrs [] [])
      , Maybe.map (\value -> small value [Attribute "class" "form-text text-muted"] [] []) input.small
      ] |> List.filterMap identity
  in
    div "" [Attribute "class" "form-group"] [] children

radioToHtmlTree input =
  let
    options = List.map (\value -> Just (toRadioOption input.id 1 value (if input.disabled then Just (Attribute "disabled" "disabled") else Nothing))) input.options
    children =
      [ Maybe.map (\value -> legend value [] [] []) input.label ]
      ++ options
      ++ [ Maybe.map (\value -> small value [Attribute "class" "form-text text-muted"] [] []) input.small ]
      |> List.filterMap identity
  in
    fieldset "" [Attribute "class" "form-group"] [] children

toRadioOption id index value disabled =
  let
    inputAttrs =
      [ Just (Attribute "type" "radio")
      , Just (Attribute "class" "form-check-input")
      , Just (Attribute "name" (toString id))
      , Just (Attribute "id" (toString id))
      , Just (Attribute "value" value)
      , disabled
      ] |> List.filterMap identity

    input = HtmlNode.input "" inputAttrs [] []
    children = label value [Attribute "class" "form-check-label"] [] [input]
  in
    div "" [ Attribute "class" "form-check" ] [] [children]


checkboxToHtmlTree box =
  let
    element = input "" [Attribute "type" "checkbox", Attribute "class" "form-check-input"] [] []
    label =
      case box.label of
        Nothing ->
          Just (HtmlNode.label "" [Attribute "class" "form-check-label"] [] [element])
        Just value ->
          Just (HtmlNode.label value [Attribute "class" "form-check-label"] [] [element])

    children = [label] |> List.filterMap identity
  in
    div "" [Attribute "class" "form-check"] [] children

buttonToHtmlTree input =
  HtmlNode.button (Maybe.withDefault "Submit" input.label) [Attribute "type" "submit", Attribute "class" "btn btn-primary"] []  []

-------------
-- Helpers --
-------------

toAddon : Maybe String -> Maybe Node
toAddon value =
  Maybe.map (\value -> div value [Attribute "class" "input-group-addon"] [] []) value

sizeClass : Size -> String
sizeClass size =
  case size of
    Small ->
      "form-control-sm"
    Normal ->
      ""
    Large ->
      "form-control-lg"
