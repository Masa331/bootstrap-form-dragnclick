module Raw exposing (build)

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

textInputToHtmlTree input =
  let
    inputAttrs =
      [ Just (Attribute "id" ("input" ++ toString input.id))
      , Maybe.map (Attribute "placeholder") input.placeholder
      , if input.disabled then Just (Attribute "disabled" "disabled") else Nothing
      , Just (Attribute "class" (String.trim ((sizeClass input.size) ++ " form-control")))
      , Just (Attribute "type" (inputTypeToString input.type_))
      ] |> List.filterMap identity

    containerClass = Attribute "class" "form-group"

    add1 = toAddon input.addon1
    add2 = toAddon input.addon2
    inputType = "input"

    input1 = Just (Element inputType inputAttrs (Children []) "" [])

    inputWithAddons =
      if List.isEmpty (List.filterMap identity [add1, add2]) then
        input1
      else
        Just (Element "div" [Attribute "class" "input-group"] (Children ([add1, input1, add2] |> List.filterMap identity)) "" [])


    children =
      [ Maybe.map (\value -> Element "label" [Attribute "for" "input1"] (Children []) value []) input.label
      , inputWithAddons
      , Maybe.map (\value -> Element "small" [Attribute "class" "form-text text-muted"] (Children []) value []) input.small
      ] |> List.filterMap identity
  in
    Element "div" [containerClass] (Children children) "" []

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
      [ Maybe.map (\value -> Element "label" [Attribute "for" "input1"] (Children []) value []) input.label
      , Just (Element "input" inputAttrs (Children []) "" [])
      , Maybe.map (\value -> Element "small" [Attribute "class" "form-text text-muted"] (Children []) value []) input.small
      ] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-group"] (Children children) "" []

selectToHtmlTree input =
  let
    inputAttrs =
      [ Just (Attribute "id" ("input" ++ toString input.id))
      , if input.disabled then Just (Attribute "disabled" "disabled") else Nothing
      , Just (Attribute "class" (String.trim ((sizeClass input.size) ++ " form-control")))
      ] |> List.filterMap identity

    options = List.map (\value -> Element "option" [] (Children []) value []) input.options
    children =
      [ Maybe.map (\value -> Element "label" [Attribute "for" "input1"] (Children []) value []) input.label
      , Just (Element "select" inputAttrs (Children options) "" [])
      , Maybe.map (\value -> Element "small" [Attribute "class" "form-text text-muted"] (Children []) value []) input.small
      ] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-group"] (Children children) "" []

textAreaToHtmlTree input =
  let
    inputAttrs =
      [ Just (Attribute "id" ("input" ++ toString input.id))
      , Maybe.map (Attribute "placeholder") input.placeholder
      , if input.disabled then Just (Attribute "disabled" "disabled") else Nothing
      , Just (Attribute "class" "form-control")
      , Just (Attribute "rows" (input.rowNumber))
      ] |> List.filterMap identity

    add1 = toAddon input.addon1
    add2 = toAddon input.addon2
    inputType = "textarea"
    input1 = Just (Element inputType inputAttrs (Children []) "" [])
    inputWithAddons =
      if List.isEmpty (List.filterMap identity [add1, add2]) then
        input1
      else
        Just (Element "div" [Attribute "class" "input-group"] (Children ([add1, input1, add2] |> List.filterMap identity)) "" [])

    children =
      [ Maybe.map (\value -> Element "label" [Attribute "for" "input1"] (Children []) value []) input.label
      , inputWithAddons
      ] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-group"] (Children (children)) "" []

multiselectToHtmlTree input =
  let
    inputAttrs =
      [ Just (Attribute "id" ("input" ++ toString input.id))
      , if input.disabled then Just (Attribute "disabled" "disabled") else Nothing
      , Just (Attribute "class" (String.trim ((sizeClass input.size) ++ " form-control")))
      , Just (Attribute "multiple" "multiple")
      ] |> List.filterMap identity

    options = List.map (\value -> Element "option" [] (Children []) value []) input.options
    children =
      [ Maybe.map (\value -> Element "label" [Attribute "for" "input1"] (Children []) value []) input.label
      , Just (Element "select" inputAttrs (Children options) "" [])
      , Maybe.map (\value -> Element "small" [Attribute "class" "form-text text-muted"] (Children []) value []) input.small
      ] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-group"] (Children children) "" []

fileUploadToHtmlTree input =
  let
    inputAttrs =
      [ Just (Attribute "id" ("input" ++ toString input.id))
      , if input.disabled then Just (Attribute "disabled" "disabled") else Nothing
      , Just (Attribute "class" (String.trim ((sizeClass input.size) ++ " form-control-file")))
      , Just (Attribute "type" "file")
      ] |> List.filterMap identity

    children =
      [ Maybe.map (\value -> Element "label" [Attribute "for" "input1"] (Children []) value []) input.label
      , Just (Element "input" inputAttrs (Children []) "" [])
      , Maybe.map (\value -> Element "small" [Attribute "class" "form-text text-muted"] (Children []) value []) input.small
      ] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-group"] (Children children) "" []

radioToHtmlTree input =
  let
    options = List.map (\value -> Just (toRadioOption input.id 1 value (if input.disabled then Just (Attribute "disabled" "disabled") else Nothing))) input.options
    children =
      [ Maybe.map (\value -> Element "legend" [] (Children []) value []) input.label ]
      ++ options
      ++ [ Maybe.map (\value -> Element "small" [Attribute "class" "form-text text-muted"] (Children []) value []) input.small ]
      |> List.filterMap identity
  in
    Element "fieldset" [Attribute "class" "form-group"] (Children children ) "" []

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


checkboxToHtmlTree input =
  let
    element = Element "input" [Attribute "type" "checkbox", Attribute "class" "form-check-input"] (Children []) "" []
    label =
      case input.label of
        Nothing ->
          Just (Element "label" [Attribute "class" "form-check-label"] (Children [element]) "" [])
        Just value ->
          Just (Element "label" [Attribute "class" "form-check-label"] (Children [element]) value [])

    children = [label] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-check"] (Children children) "" []

buttonToHtmlTree input =
  Element "button" [Attribute "type" "submit", Attribute "class" "btn btn-primary"] (Children []) (Maybe.withDefault "Submit" input.label) []

-------------
-- Helpers --
-------------

toAddon : Maybe String -> Maybe Element
toAddon value =
  Maybe.map (\value -> Element "div" [Attribute "class" "input-group-addon"] (Children []) value []) value

sizeClass : Size -> String
sizeClass size =
  case size of
    Small ->
      "form-control-sm"
    Normal ->
      ""
    Large ->
      "form-control-lg"
