module Dragged exposing (build)
import String
import Html.Events
import Html.Attributes

import HtmlTree exposing (..)
import FormModel exposing (..)
import Messages
import Models

build : Input -> HtmlTree.Element
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

textInputToHtmlTree inp =
  let
    containerClass = Attribute "class" "form-group dragged"

    children =
      [ toLabel inp.label
      , toLinks inp.id
      , wrapInAddons inp
      , Maybe.map toSmall inp.small
      ] |> List.filterMap identity
  in
    Element "div" [containerClass, Attribute "data-input-id" (toString inp.id) ] (Children children) "" []

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
      , Just (Element "input" inputAttrs (Children []) "" [])
      , Maybe.map toSmall inp.small
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    Element "div" [containerClass, Attribute "data-input-id" (toString inp.id) ] (Children children) "" []

selectToHtmlTree inp =
  let
    inputAttrs =
      [ toId inp.id
      , toDisabled inp.disabled
      , Just (Attribute "class" (String.trim ((sizeClass inp.size) ++ " form-control")))
      ] |> List.filterMap identity

    containerClass = Attribute "class" "form-group dragged"

    options = List.map (\value -> Element "option" [] (Children []) value []) inp.options
    children =
      [ toLabel inp.label
      , Just (Element "select" inputAttrs (Children options) "" [])
      , Maybe.map toSmall inp.small
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    Element "div" [containerClass, Attribute "data-input-id" (toString inp.id) ] (Children children) "" []

textAreaToHtmlTree inp =
  let
    containerClass = Attribute "class" "form-group dragged"

    children =
      [ toLabel inp.label
      , wrapInAddons inp
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    Element "div" [containerClass] (Children (children)) "" []

multiselectToHtmlTree inp =
  let
    inputAttrs =
      [ toId inp.id
      , toDisabled inp.disabled
      , Just (Attribute "class" (String.trim ((sizeClass inp.size) ++ " form-control")))
      , Just (Attribute "multiple" "multiple")
      ] |> List.filterMap identity

    containerClass = Attribute "class" "form-group dragged"

    options = List.map (\value -> Element "option" [] (Children []) value []) inp.options
    children =
      [ toLabel inp.label
      , Just (Element "select" inputAttrs (Children options) "" [])
      , Maybe.map toSmall inp.small
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    Element "div" [containerClass, Attribute "data-input-id" (toString inp.id) ] (Children children) "" []

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
      , Just (Element "input" inputAttrs (Children []) "" [])
      , Maybe.map toSmall inp.small
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    Element "div" [containerClass, Attribute "data-input-id" (toString inp.id) ] (Children children) "" []

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
    Element "fieldset" [containerClass, Attribute "data-input-id" (toString inp.id) ] (Children children ) "" []

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

    input = Element "input" inputAttrs (Children []) "" []
    children = Element "label" [Attribute "class" "form-check-label"] (Children [input]) value []
  in
    Element "div" [ Attribute "class" "form-check" ] (Children [children]) "" []


checkboxToHtmlTree inp =
  let
    input = Element "input" [Attribute "type" "checkbox", Attribute "class" "form-check-input"] (Children []) "" []
    label =
      case inp.label of
        Nothing ->
          Just (Element "label" [Attribute "class" "form-check-label"] (Children [input]) "" [])
        Just value ->
          Just (Element "label" [Attribute "class" "form-check-label"] (Children [input]) value [])

    links = toLinks inp.id
    children = [label, links] |> List.filterMap identity

    containerClass = Attribute "class" "form-check dragged"

  in
    Element "div" [containerClass, Attribute "data-input-id" (toString inp.id) ] (Children children) "" []

buttonToHtmlTree inp =
  let
    children =
      [ Just (Element "button" [Attribute "type" "submit", Attribute "class" "btn btn-primary"] (Children []) (Maybe.withDefault "Submit" inp.label) [])
      , toLinks inp.id
      ] |> List.filterMap identity

    containerClass = Attribute "class" "my-container dragged"
  in
    Element "div" [containerClass, Attribute "data-input-id" (toString inp.id) ] (Children children) "" []

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

toAddon : String -> Element
toAddon text =
  Element "div" [Attribute "class" "input-group-addon"] (Children [Element "span" [] (Children []) text []]) "" []

toSmall : String -> Element
toSmall text =
  let
    smallText = Element "span" [Attribute "class" "text-muted"] (Children []) text []
  in
    Element "small" [Attribute "class" "form-text"] (Children [smallText]) "" []

toLabel : Maybe String -> Maybe Element
toLabel value =
  let
    labelSpan = Element "span" [] (Children []) (Maybe.withDefault "" value) []
  in
    Just (Element "label" [Attribute "for" "input1"] (Children [labelSpan]) "" [])

toLegend : Maybe String -> Maybe Element
toLegend value =
  Maybe.map (\value -> Element "legend" [] (Children []) value []) value

toType : InputType -> Maybe Attribute
toType value =
  Just (Attribute "type" (inputTypeToString value))

toLinks : Int -> Maybe Element
toLinks value =
  let
    i1 = Element "i" [Attribute "class" "fa fa-arrows control-element"] (Children []) "" []
    l1 = Element "span" [] (Children [i1]) "" [Html.Events.onMouseDown ((Messages.MouseMessage (Messages.MouseClick value)))]

    children = (Children [l1])
  in
    Just (Element "div" [Attribute "class" "control-container"] children "" [])

sizeClass : Size -> String
sizeClass size =
  case size of
    Small ->
      "form-control-sm"
    Normal ->
      ""
    Large ->
      "form-control-lg"

wrapInAddons input =
  let
    inputAttrs =
      [ toId input.id
      , toPlaceholder input.placeholder
      , toDisabled input.disabled
      , Just (Attribute "class" (String.trim ((sizeClass input.size) ++ " form-control")))
      , toType input.type_
      ] |> List.filterMap identity

    add1 = Maybe.map toAddon input.addon1
    add2 = Maybe.map toAddon input.addon2
    inputType =
      case input.type_ of
        TextArea -> "textarea"
        _ -> "input"

    input1 = Just (Element inputType inputAttrs (Children []) "" [])
    inputClasses =
      case input.size of
        Small ->
          "input-group input-group-sm"
        Normal ->
          "input-group"
        Large ->
          "input-group input-group-lg"
  in
    Just (Element "div" [Attribute "class" inputClasses] (Children ([add1, input1, add2] |> List.filterMap identity)) "" [])
