module ForInputEdit exposing (build)
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

textInputToHtmlTree inp =
  let
    containerClass =
      [ Just "form-group"
      , Just "show-hidden-on-hover"
      , if inp.dragged then Just "hidden" else Nothing
      ] |> List.filterMap identity
        |> String.join " "
        |> Attribute "class"

    children =
      [ toLabel inp.label
      , toLinks inp.id
      , wrapInAddons inp
      , Maybe.map toSmall inp.small
      ] |> List.filterMap identity
  in
    div "" [containerClass, Attribute "data-input-id" (toString inp.id) ] [] children

colorToHtmlTree inp =
  let
    inputAttrs =
      [ toId inp.id
      , toPlaceholder inp.placeholder
      , toDisabled inp.disabled
      , Just (Attribute "class" (String.trim ((sizeClass inp.size) ++ " form-control")))
      , toType inp.type_
      ] |> List.filterMap identity

    containerClass =
      [ Just "form-group"
      , Just "show-hidden-on-hover"
      , if inp.dragged then Just "hidden" else Nothing
      ] |> List.filterMap identity
        |> String.join " "
        |> Attribute "class"

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

    containerClass =
      [ Just "form-group"
      , Just "show-hidden-on-hover"
      , if inp.dragged then Just "hidden" else Nothing
      ] |> List.filterMap identity
        |> String.join " "
        |> Attribute "class"

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
    containerClass =
      [ Just "form-group"
      , Just "show-hidden-on-hover"
      , if inp.dragged then Just "hidden" else Nothing
      ] |> List.filterMap identity
        |> String.join " "
        |> Attribute "class"

    children =
      [ toLabel inp.label
      , wrapInAddons inp
      , toLinks inp.id
      , Maybe.map toSmall inp.small
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

    containerClass =
      [ Just "form-group"
      , Just "show-hidden-on-hover"
      , if inp.dragged then Just "hidden" else Nothing
      ] |> List.filterMap identity
        |> String.join " "
        |> Attribute "class"

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

    containerClass =
      [ Just "form-group"
      , Just "show-hidden-on-hover"
      , if inp.dragged then Just "hidden" else Nothing
      ] |> List.filterMap identity
        |> String.join " "
        |> Attribute "class"

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
      [ Just (toLegend inp) ]
      ++ options
      ++ [ Maybe.map toSmall inp.small ]
      |> List.filterMap identity

    containerClass =
      [ Just "form-group"
      , Just "show-hidden-on-hover"
      , if inp.dragged then Just "hidden" else Nothing
      ] |> List.filterMap identity
        |> String.join " "
        |> Attribute "class"
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
    -- This (" " ++ value) is nasty hack. I don't know what to do but elm generated fonts miss tiny space
    --   between actuall input and label although the markup is same with static html - remove the space
    --   to see it
    children = label (" " ++ value) [Attribute "class" "form-check-label"] [] [input]
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
          -- This (" " ++ value) is nasty hack. I don't know what to do but elm generated fonts miss tiny space
          --   between actuall input and label although the markup is same with static html - remove the space
          --   to see it
          Just (HtmlNode.label (" " ++ value) [Attribute "class" "form-check-label"] [] [input])

    links = toLinks inp.id
    small = Maybe.map toSmall inp.small
    children = [label, links, small] |> List.filterMap identity

    containerClass =
      [ Just "form-check"
      , Just "show-hidden-on-hover"
      , if inp.dragged then Just "hidden" else Nothing
      ] |> List.filterMap identity
        |> String.join " "
        |> Attribute "class"
  in
    div "" [containerClass, Attribute "data-input-id" (toString inp.id) ] [] children

buttonToHtmlTree inp =
  let
    sizeClass =
      case inp.size of
        Small -> " btn-sm"
        Normal -> ""
        Large -> " btn-lg"

    children =
      [ Just (HtmlNode.button (Maybe.withDefault "Submit" inp.label) [Attribute "type" "submit", Attribute "class" ("btn btn-primary" ++ sizeClass)] [] [])
      , toLinks inp.id
      ] |> List.filterMap identity

    containerClass =
      [ Just "my-container"
      , Just "show-hidden-on-hover"
      , if inp.dragged then Just "hidden" else Nothing
      ] |> List.filterMap identity
        |> String.join " "
        |> Attribute "class"
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
    Just (label "" [Attribute "for" "input1"] [] [labelSpan])

toLegend : Input -> Node
toLegend input =
  let
    i4 = i "" [Attribute "class" "fa fa-check control-element"] [] []
    l4 = span "" [] [Html.Events.onClick ((Messages.InputMessage (Messages.ToggleDisabled input.id)))] [i4]
    divider = span " " [] [] []

    children = [ l4 ]
    links = span "" [Attribute "class" "hidden-inherit float-right one-rem-size"] [] children
    label = span (Maybe.withDefault "" input.label) [] [] []
  in
    legend "" [] [] [label, links]

toType : InputType -> Maybe Attribute
toType value =
  Just (Attribute "type" (inputTypeToString value))

toLinks : Int -> Maybe Node
toLinks value =
  let
    i1 = i "" [Attribute "class" "fa fa-font fa-small control-element"] [] []
    l1 = span "" [] [Html.Events.onClick ((Messages.InputMessage (Messages.SizeEdit value "small")))] [i1]
    i2 = i "" [Attribute "class" "fa fa-font fa-normal control-element"] [] []
    l2 = span "" [] [Html.Events.onClick ((Messages.InputMessage (Messages.SizeEdit value "normal")))] [i2]
    i3 = i "" [Attribute "class" "fa fa-font fa-big control-element"] [] []
    l3 = span "" [] [Html.Events.onClick ((Messages.InputMessage (Messages.SizeEdit value "large")))] [i3]

    i4 = i "" [Attribute "class" "fa fa-check control-element"] [] []
    l4 = span "" [] [Html.Events.onClick ((Messages.InputMessage (Messages.ToggleDisabled value)))] [i4]
    divider = span " " [] [] []

    children = [ l1, divider, l2, divider, l3, divider, l4 ]
  in
    Just (div "" [Attribute "class" "control-container hidden-block"] [] children)

sizeClass : Size -> String
sizeClass size =
  case size of
    Small ->
      "form-control-sm"
    Normal ->
      ""
    Large ->
      "form-control-lg"

toRowNumber : Input -> Maybe Attribute
toRowNumber input =
  case input.type_ of
    TextArea ->
      Just (Attribute "rows" input.rowNumber)
    _ ->
      Nothing

wrapInAddons input =
  let
    inputAttrs =
      [ toId input.id
      , toPlaceholder input.placeholder
      , toRowNumber input
      , toDisabled input.disabled
      , Just (Attribute "class" (String.trim ((sizeClass input.size) ++ " form-control")))
      , toType input.type_
      ] |> List.filterMap identity

    add1 = Maybe.map toAddon input.addon1
    add2 = Maybe.map toAddon input.addon2

    input1 =
      case input.type_ of
        TextArea -> Just (textarea "" inputAttrs [] [])
        _ -> Just (HtmlNode.input "" inputAttrs [] [])

    inputClasses =
      case input.size of
        Small ->
          "input-group input-group-sm"
        Normal ->
          "input-group"
        Large ->
          "input-group input-group-lg"
  in
    Just (div "" [Attribute "class" inputClasses] [] ([add1, input1, add2] |> List.filterMap identity))
