module InputToHtmlTreeConverter exposing (..)

import HtmlTree exposing (..)
import FormModel exposing (..)

import String

inputToHtmlTree input =
  case input.type' of
    Text -> textInputToHtmlTree input
    TextArea -> textAreaToHtmlTree input
    Select -> selectToHtmlTree input
    Multiselect -> multiselectToHtmlTree input
    FileUpload -> fileUploadToHtmlTree input
    Radio -> radioToHtmlTree input
    Checkbox -> checkboxToHtmlTree input
    Button -> buttonToHtmlTree input
    _ -> textInputToHtmlTree input

textInputToHtmlTree inp =
  let
    inputAttrs =
      [ toId inp.id
      , toPlaceholder inp.placeholder
      , toDisabled inp.disabled
      , toReadonly inp.readonly
      , toClasses inp.classList inp.size
      , toType inp.type'
      ] |> List.filterMap identity

    children =
      [ toLabel inp.label
      , wrapInAddons inputAttrs inp
      , toSmall inp.small
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-group"] (Children children) ""

selectToHtmlTree inp =
  let
    inputAttrs =
      [ toId inp.id
      , toDisabled inp.disabled
      , toClasses inp.classList inp.size
      ] |> List.filterMap identity

    options = List.map (\value -> Element "option" [] (Children []) value) inp.options
    children =
      [ toLabel inp.label
      , Just (Element "select" inputAttrs (Children options) "")
      , toSmall inp.small
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-group"] (Children children) ""

textAreaToHtmlTree inp =
  let
    inputAttrs =
      [ toId inp.id
      , toPlaceholder inp.placeholder
      , toDisabled inp.disabled
      , toReadonly inp.readonly
      , toClasses inp.classList Normal
      , Just (Attribute "rows" (toString inp.rowNumber))
      ] |> List.filterMap identity

    children =
      [ toLabel inp.label
      , Just (Element "textarea" inputAttrs (Children []) "")
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-group"] (Children (children)) ""

multiselectToHtmlTree inp =
  let
    inputAttrs =
      [ toId inp.id
      , toDisabled inp.disabled
      , toClasses inp.classList Normal
      , Just (Attribute "multiple" "multiple")
      ] |> List.filterMap identity

    options = List.map (\value -> Element "option" [] (Children []) value) inp.options
    children =
      [ toLabel inp.label
      , Just (Element "select" inputAttrs (Children options) "")
      , toSmall inp.small
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-group"] (Children children) ""

fileUploadToHtmlTree inp =
  let
    inputAttrs =
      [ toId inp.id
      , toDisabled inp.disabled
      , toClasses inp.classList Normal
      , Just (Attribute "type" "file")
      ] |> List.filterMap identity

    children =
      [ toLabel inp.label
      , Just (Element "input" inputAttrs (Children []) "")
      , toSmall inp.small
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-group"] (Children children) ""

radioToHtmlTree inp =
  let
    options = List.map (\value -> Just (toRadioOption inp.id 1 value)) inp.options
    children =
      [ toLegend inp.label ]
      ++ options
      ++ [ toLinks inp.id ]
      |> List.filterMap identity
  in
    Element "fieldset" [Attribute "class" "form-group"] (Children children ) ""

toRadioOption id index value =
  let
    input = Element "input" [Attribute "type" "radio", Attribute "class" "form-check-input", Attribute "name" (toString id), Attribute "id" (toString id), Attribute "value" value] (Children []) ""
    children = Element "label" [Attribute "class" "form-check-label"] (Children [input]) value
  in
    Element "div" [Attribute "class" "form-check"] (Children [children]) ""


checkboxToHtmlTree inp =
  let
    input = Element "input" [Attribute "type" "checkbox", Attribute "class" "form-check-input"] (Children []) ""
    label =
      case inp.label of
        Nothing ->
          Just (Element "label" [Attribute "class" "form-check-label"] (Children [input]) "")
        Just value ->
          Just (Element "label" [Attribute "class" "form-check-label"] (Children [input]) value)

    links = toLinks inp.id
    children = [label, links] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-check"] (Children children) ""

buttonToHtmlTree inp =
  let
    children =
      [ Just (Element "button" [Attribute "type" "submit", Attribute "class" "btn btn-primary"] (Children []) (Maybe.withDefault "Submit" inp.label))
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "my-container"] (Children children) ""

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

toReadonly : Bool -> Maybe Attribute
toReadonly value =
  if value then Just (Attribute "readonly" "readonly") else Nothing

toAddon : Maybe String -> Maybe Element
toAddon value =
  Maybe.map (\value -> Element "div" [Attribute "class" "input-group-addon"] (Children []) value) value

toSmall : Maybe String -> Maybe Element
toSmall value =
  Maybe.map (\value -> Element "small" [Attribute "class" "form-text text-muted"] (Children []) value) value

toLabel : Maybe String -> Maybe Element
toLabel value =
  Maybe.map (\value -> Element "label" [Attribute "for" "input1"] (Children []) value) value

toLegend : Maybe String -> Maybe Element
toLegend value =
  Maybe.map (\value -> Element "legend" [] (Children []) value) value

toType : InputType -> Maybe Attribute
toType value =
  Just (Attribute "type" (typeToText value))

toLinks : Id -> Maybe Element
toLinks value =
  Just (Element "editLinks" [] (Children []) (toString value))

toClasses : List String -> Size -> Maybe Attribute
toClasses classList size =
  let
    sizeClass =
      case size of
        Small -> "form-control-sm"
        Normal -> ""
        Large -> "form-control-lg"
  in
    Just (Attribute "class" (String.join " " (sizeClass::classList)))

wrapInAddons inputAttrs attrs =
  let
    add1 = toAddon attrs.addon1
    add2 = toAddon attrs.addon2
    input1 = Just (Element "input" inputAttrs (Children []) "")
  in
    if List.isEmpty (List.filterMap identity [add1, add2]) then
      input1
    else
      Just (Element "div" [Attribute "class" "input-group"] (Children ([add1, input1, add2] |> List.filterMap identity)) "")
