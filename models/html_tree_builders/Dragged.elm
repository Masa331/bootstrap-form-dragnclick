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
    inputAttrs =
      [ toId inp.id
      , toPlaceholder inp.placeholder
      , toDisabled inp.disabled
      , toClasses ((sizeClass inp.size) :: ([ "form-control" ] ++ inp.classList))
      , toType inp.type_
      ] |> List.filterMap identity

    containerClass =
      [ Just "form-group"
      , if inp.dragged then Just "dragged" else Nothing
      ] |> List.filterMap identity
        |> String.join " "
        |> Attribute "class"

    children =
      [ toLabel inp.label
      , wrapInAddons inputAttrs inp
      , toSmall inp.small
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    Element "div" [containerClass, Attribute "data-input-id" (toString inp.id) ] (Children children) "" []

colorToHtmlTree inp =
  let
    inputAttrs =
      [ toId inp.id
      , toPlaceholder inp.placeholder
      , toDisabled inp.disabled
      , toClasses ((sizeClass inp.size) :: [ "form-control" ])
      , toType inp.type_
      ] |> List.filterMap identity

    children =
      [ toLabel inp.label
      , Just (Element "input" inputAttrs (Children []) "" [])
      , toSmall inp.small
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-group", Attribute "data-input-id" (toString inp.id) ] (Children children) "" []

selectToHtmlTree inp =
  let
    inputAttrs =
      [ toId inp.id
      , toDisabled inp.disabled
      , toClasses ((sizeClass inp.size) :: [ "form-control" ])
      ] |> List.filterMap identity

    options = List.map (\value -> Element "option" [] (Children []) value []) inp.options
    children =
      [ toLabel inp.label
      , Just (Element "select" inputAttrs (Children options) "" [])
      , toSmall inp.small
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-group", Attribute "data-input-id" (toString inp.id) ] (Children children) "" []

textAreaToHtmlTree inp =
  let
    inputAttrs =
      [ toId inp.id
      , toPlaceholder inp.placeholder
      , toDisabled inp.disabled
      , toClasses [ "form-control" ]
      , Just (Attribute "rows" (inp.rowNumber))
      ] |> List.filterMap identity

    children =
      [ toLabel inp.label
      , wrapInAddons inputAttrs inp
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-group"] (Children (children)) "" []

multiselectToHtmlTree inp =
  let
    inputAttrs =
      [ toId inp.id
      , toDisabled inp.disabled
      , toClasses ((sizeClass inp.size) :: [ "form-control" ])
      , Just (Attribute "multiple" "multiple")
      ] |> List.filterMap identity

    options = List.map (\value -> Element "option" [] (Children []) value []) inp.options
    children =
      [ toLabel inp.label
      , Just (Element "select" inputAttrs (Children options) "" [])
      , toSmall inp.small
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-group", Attribute "data-input-id" (toString inp.id) ] (Children children) "" []

fileUploadToHtmlTree inp =
  let
    inputAttrs =
      [ toId inp.id
      , toDisabled inp.disabled
      , toClasses ((sizeClass inp.size) :: inp.classList)
      , Just (Attribute "type" "file")
      ] |> List.filterMap identity

    children =
      [ toLabel inp.label
      , Just (Element "input" inputAttrs (Children []) "" [])
      , toSmall inp.small
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-group", Attribute "data-input-id" (toString inp.id) ] (Children children) "" []

radioToHtmlTree inp =
  let
    options = List.map (\value -> Just (toRadioOption inp.id 1 value (toDisabled inp.disabled))) inp.options
    children =
      [ toLegend inp.label ]
      ++ options
      ++ [ toSmall inp.small ]
      ++ [ toLinks inp.id ]
      |> List.filterMap identity
  in
    Element "fieldset" [Attribute "class" "form-group", Attribute "data-input-id" (toString inp.id) ] (Children children ) "" []

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
  in
    Element "div" [Attribute "class" "form-check", Attribute "data-input-id" (toString inp.id) ] (Children children) "" []

buttonToHtmlTree inp =
  let
    children =
      [ Just (Element "button" [Attribute "type" "submit", Attribute "class" "btn btn-primary"] (Children []) (Maybe.withDefault "Submit" inp.label) [])
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "my-container", Attribute "data-input-id" (toString inp.id) ] (Children children) "" []

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

toAddon : Maybe String -> Maybe Element
toAddon value =
  Maybe.map (\value -> Element "div" [Attribute "class" "input-group-addon"] (Children []) value []) value

toSmall : Maybe String -> Maybe Element
toSmall value =
  Maybe.map (\value -> Element "small" [Attribute "class" "form-text text-muted"] (Children []) value []) value

toLabel : Maybe String -> Maybe Element
toLabel value =
  Maybe.map (\value -> Element "label" [Attribute "for" "input1"] (Children []) value []) value

toLegend : Maybe String -> Maybe Element
toLegend value =
  Maybe.map (\value -> Element "legend" [] (Children []) value []) value

toType : InputType -> Maybe Attribute
toType value =
  Just (Attribute "type" (inputTypeToString value))

toLinks : Id -> Maybe Element
toLinks value =
  let
    i3 = Element "i" [Attribute "class" "fa fa-arrows control"] (Children []) "" []
    l3 = Element "span" [] (Children [i3]) "" [Html.Events.onMouseDown ((Messages.MouseMessage (Messages.MouseClick value)))]

    children = (Children [l3])
  in
    Just (Element "div" [Attribute "class" "edit-and-remove-link"] children "" [])

sizeClass : Size -> String
sizeClass size =
  case size of
    Small ->
      "form-control-sm"
    Normal ->
      ""
    Large ->
      "form-control-lg"

toClasses : List String -> Maybe Attribute
toClasses classList =
  let
    value =
      List.filter (\class -> class /= "") classList
        |> String.join " "
  in
    Just (Attribute "class" value)

wrapInAddons inputAttrs input =
  let
    add1 = toAddon input.addon1
    add2 = toAddon input.addon2
    inputType =
      case input.type_ of
        TextArea -> "textarea"
        _ -> "input"
    input1 = Just (Element inputType inputAttrs (Children []) "" [])
  in
    if List.isEmpty (List.filterMap identity [add1, add2]) then
      input1
    else
      Just (Element "div" [Attribute "class" "input-group"] (Children ([add1, input1, add2] |> List.filterMap identity)) "" [])
