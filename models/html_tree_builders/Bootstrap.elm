module Bootstrap exposing (..)
import HtmlNode exposing (..)
import Models
import Inputs exposing (..)

import Html
import Messages
import Html.Events exposing (onClick, onMouseDown)

colorToHtmlNode input containerClass links =
  let
    inputAttrs =
      [ Just (Attribute "id" ("input" ++ toString input.id))
      , Maybe.map (Attribute "placeholder") input.placeholder
      , if input.disabled then Just (Attribute "disabled" "disabled") else Nothing
      , Just (Attribute "class" (String.trim ((sizeClass input.size) ++ " form-control")))
      , Just (Attribute "type" (inputTypeToString input.type_))
      ] |> List.filterMap identity
  in
    div "" [ containerClass, Attribute "data-input-id" (toString input.id) ] []
      ([ toLabel input.label
      , Just (HtmlNode.input "" inputAttrs [] [])
      , Maybe.map toSmall input.small
      , Just links
      ] |> List.filterMap identity)

multiselectToHtmlNode input containerClass links =
  let
    inputAttrs =
      [ Just (Attribute "id" ("input" ++ toString input.id))
      , if input.disabled then Just (Attribute "disabled" "disabled") else Nothing
      , Just (Attribute "class" (String.trim ((sizeClass input.size) ++ " form-control")))
      , Just (Attribute "multiple" "multiple")
      ] |> List.filterMap identity

    options = List.map (\value -> option value [] [] []) input.options
  in
    div "" [containerClass, Attribute "data-input-id" (toString input.id) ] []
      ([ toLabel input.label
       , Just (select "" inputAttrs [] options)
       , Maybe.map toSmall input.small
       , Just links
       ] |> List.filterMap identity)

fileUploadToHtmlNode input containerClass links =
  let
    inputAttrs =
      [ Just (Attribute "id" ("input" ++ toString input.id))
      , if input.disabled then Just (Attribute "disabled" "disabled") else Nothing
      , Just (Attribute "class" (String.trim ((sizeClass input.size) ++ " form-control-file")))
      , Just (Attribute "type" "file")
      ] |> List.filterMap identity
  in
    div "" [containerClass, Attribute "data-input-id" (toString input.id) ] []
      ([ toLabel input.label
       , Just (HtmlNode.input "" inputAttrs [] [])
       , Maybe.map toSmall input.small
       , Just links
       ] |> List.filterMap identity)

buttonToHtmlNode input containerClass links =
  let
    sizeClass =
      case input.size of
        Small -> " btn-sm"
        Normal -> ""
        Large -> " btn-lg"

    inputAttrs =
      [ Just (Attribute "id" ("input" ++ toString input.id))
      , if input.disabled then Just (Attribute "disabled" "disabled") else Nothing
      , Just (Attribute "class" (String.trim ("btn btn-primary" ++ sizeClass)))
      , Just (Attribute "type" "submit")
      ] |> List.filterMap identity
  in
    div "" [containerClass, Attribute "data-input-id" (toString input.id) ] []
      ([ Just (HtmlNode.button (Maybe.withDefault "Submit" input.label) inputAttrs [] [])
       , Just links
       ] |> List.filterMap identity)

checkboxToHtmlNode input containerClass links =
  let
    inputAttrs =
      [ Just (Attribute "id" ("input" ++ toString input.id))
      , if input.disabled then Just (Attribute "disabled" "disabled") else Nothing
      , Just (Attribute "class" "form-check-input")
      , Just (Attribute "type" (inputTypeToString input.type_))
      ] |> List.filterMap identity

    inputNode = HtmlNode.input "" inputAttrs [] []
    label =
      case input.label of
        Nothing ->
          Just (HtmlNode.label "" [Attribute "class" "form-check-label"] [] [inputNode])
        Just value ->
          -- This (" " ++ value) is nasty hack. I don't know what to do but elm generated fonts miss tiny space
          --   between actuall input and label although the markup is same with static html - remove the space
          --   to see it
          Just (HtmlNode.label (" " ++ value) [Attribute "class" "form-check-label"] [] [inputNode])

    small = Maybe.map toSmall input.small
  in
    div "" [containerClass, Attribute "data-input-id" (toString input.id) ] []
      ([ label
       , Just links
       , small ] |> List.filterMap identity)

radioToHtmlNode input containerClass legend =
  let
    disabled = if input.disabled then Just (Attribute "disabled" "disabled") else Nothing
    options = List.map (\value -> Just (toRadioOption input.id 1 value disabled)) input.options
  in
    fieldset "" [containerClass, Attribute "data-input-id" (toString input.id) ] []
      ([ legend ]
        ++ options
        ++ [ Maybe.map toSmall input.small ]
        |> List.filterMap identity)

textAreaToHtmlNode input containerClass links =
  let
    inputAttrs =
      [ Just (Attribute "id" ("input" ++ toString input.id))
      , Maybe.map (Attribute "placeholder") input.placeholder
      , Just (Attribute "rows" input.rowNumber)
      , if input.disabled then Just (Attribute "disabled" "disabled") else Nothing
      , Just (Attribute "class" (String.trim ((sizeClass input.size) ++ " form-control")))
      , Just (Attribute "type" (inputTypeToString input.type_))
      ] |> List.filterMap identity

    add1 = Maybe.map toAddon input.addon1
    add2 = Maybe.map toAddon input.addon2
    inputType = "textarea"

    input1 = Just (textarea "" inputAttrs [] [])
    inputClasses =
      case input.size of
        Small ->
          "input-group input-group-sm"
        Normal ->
          "input-group"
        Large ->
          "input-group input-group-lg"
    inputGroup = Just (div "" [Attribute "class" inputClasses] [] ([add1, input1, add2] |> List.filterMap identity))

    children =
      [ toLabel input.label
      , inputGroup
      , Just links
      , Maybe.map toSmall input.small
      ] |> List.filterMap identity
  in
    div "" [containerClass, Attribute "data-input-id" (toString input.id) ] [] children

selectToHtmlNode input containerClass links =
  let
    inputAttrs =
      [ Just (Attribute "id" ("input" ++ toString input.id))
      , if input.disabled then Just (Attribute "disabled" "disabled") else Nothing
      , Just (Attribute "class" (String.trim ((sizeClass input.size) ++ " form-control")))
      ] |> List.filterMap identity

    options = List.map (\value -> option value [] [] []) input.options
    children =
      [ toLabel input.label
      , Just (select "" inputAttrs [] options)
      , Maybe.map toSmall input.small
      , Just links
      ] |> List.filterMap identity
  in
    div "" [containerClass, Attribute "data-input-id" (toString input.id) ] [] children

textInputToHtmlNode input containerClass links =
  let
    inputAttrs =
      [ Just (Attribute "id" ("input" ++ toString input.id))
      , Maybe.map (Attribute "placeholder") input.placeholder
      , if input.disabled then Just (Attribute "disabled" "disabled") else Nothing
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
      , Just links
      , inputGroup
      , Maybe.map toSmall input.small
      ] |> List.filterMap identity
  in
    div "" [containerClass, Attribute "data-input-id" (toString input.id) ] [] children

-------------
-- Helpers --
-------------

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

sizeClass : Size -> String
sizeClass size =
  case size of
    Small ->
      "form-control-sm"
    Normal ->
      ""
    Large ->
      "form-control-lg"

toAddon : String -> Node
toAddon text =
  div "" [Attribute "class" "input-group-addon"] [] [span text [] [] []]

linksDivider : Node
linksDivider =
  span " " [] [] []

editLink : Input -> Node
editLink input =
  a "" [Attribute "href" ("#input/" ++ toString input.id)] []
    [ i "" [Attribute "class" "fa fa-edit control-element"] [] [] ]

sizeLinkSmall : Input -> Node
sizeLinkSmall input =
  iconLink "fa-font fa-small" (onClick (Messages.InputMessage (Messages.SizeEdit input.id "small")))

sizeLinkNormal : Input -> Node
sizeLinkNormal input =
  iconLink "fa-font fa-normal" (onClick (Messages.InputMessage (Messages.SizeEdit input.id "normal")))

sizeLinkLarge : Input -> Node
sizeLinkLarge input =
  iconLink "fa-font fa-big" (onClick (Messages.InputMessage (Messages.SizeEdit input.id "large")))

deleteLink : Input -> Node
deleteLink input =
  iconLink "fa-trash" (onClick (Messages.FormMessage (Messages.RemoveInput input.id)))

disabledLink : Input -> Node
disabledLink input =
  iconLink "fa-check" (onClick (Messages.InputMessage (Messages.ToggleDisabled input.id)))

dragLink : Input -> Node
dragLink input =
  iconLink "fa-arrows" (onMouseDown (Messages.MouseMessage (Messages.MouseClick input.id)))

iconLink : String -> (Html.Attribute Messages.Msg) -> Node
iconLink class event =
  let
    icon = i "" [Attribute "class" ("fa control-element " ++ class)] [] []
  in
    span "" [] [event] [icon]
