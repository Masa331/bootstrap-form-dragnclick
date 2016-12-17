module Bootstrap exposing (..)
import HtmlNode exposing (..)
import Models
import Inputs exposing (..)

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
      , links
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
       , links
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
       , links
       ] |> List.filterMap identity)

-------------
-- Helpers --
-------------

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
