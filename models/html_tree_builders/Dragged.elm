module Dragged exposing (build)
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
    _ -> textInputToHtmlTree input

-------------
-- Private --
-------------

textInputToHtmlTree input =
  let
    containerClass = Attribute "class" "form-group dragged"

    inputAttrs =
      [ Just (Attribute "id" ("input" ++ toString input.id))
      , Maybe.map (Attribute "placeholder") input.placeholder
      , toDisabled input.disabled
      , Just (Attribute "class" (String.trim ((sizeClass input.size) ++ " form-control")))
      , Just (Attribute "type" (inputTypeToString input.type_))
      ] |> List.filterMap identity

    add1 = Maybe.map toAddon input.addon1
    add2 = Maybe.map toAddon input.addon2

    input1 = Just (HtmlNode.input "" inputAttrs [] [])
    inputClasses =
      case input.size of
        Small ->
          "input-group input-group-sm"
        Normal ->
          "input-group"
        Large ->
          "input-group input-group-lg"

    inputGroup =
      Just (div "" [Attribute "class" inputClasses] [] ([add1, input1, add2] |> List.filterMap identity))

    children =
      [ toLabel input.label
      , toLinks input.id
      , inputGroup
      , Maybe.map toSmall input.small
      ] |> List.filterMap identity
  in
    div "" [containerClass, Attribute "data-input-id" (toString input.id) ] [] children

colorToHtmlTree inp =
  let
    inputAttrs =
      [ toId inp.id
      , toPlaceholder inp.placeholder
      , toDisabled inp.disabled
      , Just (Attribute "class" (String.trim ((sizeClass inp.size) ++ " form-control")))
      , toType inp.type_
      ] |> List.filterMap identity

    containerClass = Attribute "class" "form-group dragged"

    children =
      [ toLabel inp.label
      , Just (input "" inputAttrs [] [])
      , Maybe.map toSmall inp.small
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    div "" [containerClass, Attribute "data-input-id" (toString inp.id) ] [] children

selectToHtmlTree inp =
  let
    inputAttrs =
      [ toId inp.id
      , toDisabled inp.disabled
      , Just (Attribute "class" (String.trim ((sizeClass inp.size) ++ " form-control")))
      ] |> List.filterMap identity

    containerClass = Attribute "class" "form-group dragged"

    options = List.map (\value -> option value [] [] []) inp.options
    children =
      [ toLabel inp.label
      , Just (select "" inputAttrs [] options)
      , Maybe.map toSmall inp.small
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    div "" [containerClass, Attribute "data-input-id" (toString inp.id) ] [] children

textAreaToHtmlTree inp =
  let
    containerClass = Attribute "class" "form-group dragged"

    inputAttrs =
      [ toId inp.id
      , toPlaceholder inp.placeholder
      , toDisabled inp.disabled
      , Just (Attribute "class" (String.trim ((sizeClass inp.size) ++ " form-control")))
      , toType inp.type_
      ] |> List.filterMap identity

    add1 = Maybe.map toAddon inp.addon1
    add2 = Maybe.map toAddon inp.addon2

    input1 = Just (textarea "" inputAttrs [] [])
    inputClasses =
      case inp.size of
        Small ->
          "input-group input-group-sm"
        Normal ->
          "input-group"
        Large ->
          "input-group input-group-lg"
    inputGroup =
      Just (div "" [Attribute "class" inputClasses] [] ([add1, input1, add2] |> List.filterMap identity))

    children =
      [ toLabel inp.label
      , inputGroup
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    div "" [containerClass] [] children

multiselectToHtmlTree inp =
  let
    inputAttrs =
      [ toId inp.id
      , toDisabled inp.disabled
      , Just (Attribute "class" (String.trim ((sizeClass inp.size) ++ " form-control")))
      , Just (Attribute "multiple" "multiple")
      ] |> List.filterMap identity

    containerClass = Attribute "class" "form-group dragged"

    options = List.map (\value -> option value [] [] []) inp.options
    children =
      [ toLabel inp.label
      , Just (select "" inputAttrs [] options)
      , Maybe.map toSmall inp.small
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    div "" [containerClass, Attribute "data-input-id" (toString inp.id) ] [] children

fileUploadToHtmlTree inp =
  let
    inputAttrs =
      [ toId inp.id
      , toDisabled inp.disabled
      , Just (Attribute "class" (String.trim ((sizeClass inp.size) ++ " form-control-file")))
      , Just (Attribute "type" "file")
      ] |> List.filterMap identity

    containerClass = Attribute "class" "form-group dragged"

    children =
      [ toLabel inp.label
      , Just (input "" inputAttrs [] [])
      , Maybe.map toSmall inp.small
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    div "" [containerClass, Attribute "data-input-id" (toString inp.id) ] [] children

radioToHtmlTree inp =
  let
    options = List.map (\value -> Just (toRadioOption inp.id 1 value (toDisabled inp.disabled))) inp.options
    children =
      [ toLegend inp.label ]
      ++ options
      ++ [ Maybe.map toSmall inp.small ]
      ++ [ toLinks inp.id ]
      |> List.filterMap identity

    containerClass = Attribute "class" "form-group dragged"
  in
    fieldset "" [containerClass, Attribute "data-input-id" (toString inp.id) ] [] children

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


checkboxToHtmlTree inp =
  let
    input = HtmlNode.input "" [Attribute "type" "checkbox", Attribute "class" "form-check-input"] [] []
    label =
      case inp.label of
        Nothing ->
          Just (HtmlNode.label "" [Attribute "class" "form-check-label"] [] [input])
        Just value ->
          Just (HtmlNode.label value [Attribute "class" "form-check-label"] [] [input])

    links = toLinks inp.id
    children = [label, links] |> List.filterMap identity

    containerClass = Attribute "class" "form-check dragged"

  in
    div "" [containerClass, Attribute "data-input-id" (toString inp.id) ] [] children

buttonToHtmlTree inp =
  let
    children =
      [ Just (HtmlNode.button(Maybe.withDefault "Submit" inp.label) [Attribute "type" "submit", Attribute "class" "btn btn-primary"] [] [])
      , toLinks inp.id
      ] |> List.filterMap identity

    containerClass = Attribute "class" "my-container dragged"
  in
    div "" [containerClass, Attribute "data-input-id" (toString inp.id) ] [] children

-------------
-- Helpers --
-------------

toPlaceholder : Maybe String -> Maybe Attribute
toPlaceholder value =
  Maybe.map (Attribute "placeholder") value

toId : Int -> Maybe Attribute
toId value =
  Just (Attribute "id" ("input" ++ toString value))

toDisabled : Bool -> Maybe Attribute
toDisabled value =
  if value then Just (Attribute "disabled" "disabled") else Nothing

toAddon : String -> Node
toAddon text =
  div "" [Attribute "class" "input-group-addon"] [] [span text [] [] []]

toSmall : String -> Node
toSmall text =
  let
    smallText = span text [Attribute "class" "text-muted"] [] []
  in
    small "" [Attribute "class" "form-text"] [] [smallText]

toLabel : Maybe String -> Maybe Node
toLabel value =
  let
    labelSpan = span (Maybe.withDefault "" value) [] [] []
  in
    Just (HtmlNode.label "" [Attribute "for" "input1"] [] [labelSpan])

toLegend : Maybe String -> Maybe Node
toLegend value =
  Maybe.map (\value -> legend value [] [] []) value

toType : InputType -> Maybe Attribute
toType value =
  Just (Attribute "type" (inputTypeToString value))

toLinks : Int -> Maybe Node
toLinks value =
  let
    i1 = i "" [Attribute "class" "fa fa-arrows control-element"] [] []
    l1 = span "" [] [Html.Events.onMouseDown ((Messages.MouseMessage (Messages.MouseClick value)))] [i1]

    children = [l1]
  in
    Just (div "" [Attribute "class" "control-container"] [] children)

sizeClass : Size -> String
sizeClass size =
  case size of
    Small ->
      "form-control-sm"
    Normal ->
      ""
    Large ->
      "form-control-lg"
