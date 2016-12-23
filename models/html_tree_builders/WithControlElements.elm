module WithControlElements exposing (build)

import Html
import Html.Events exposing (onClick, onMouseDown)
import HtmlNode exposing (..)
import Inputs exposing (..)
import Messages
import Bootstrap

build : Input -> HtmlNode.Node
build input =
  let
    links = toLinks input.id
    containerClass =
      Attribute "class" ("form-group show-hidden-on-hover" ++ if input.dragged then " hidden" else "")
  in
    case input.type_ of
      Text ->
        Bootstrap.textInputToHtmlNode input containerClass links
      TextArea ->
        Bootstrap.textAreaToHtmlNode input containerClass links
      Select ->
        Bootstrap.selectToHtmlNode input containerClass links
      Multiselect ->
        Bootstrap.multiselectToHtmlNode input containerClass links
      FileUpload ->
        Bootstrap.fileUploadToHtmlNode input containerClass links
      Radio ->
        Bootstrap.radioToHtmlNode input containerClass (Just (toLegend input))
      Checkbox ->
        Bootstrap.checkboxToHtmlNode input
          (Attribute "class" ("form-check show-hidden-on-hover" ++ if input.dragged then " hidden" else ""))
          links
      Button ->
        Bootstrap.buttonToHtmlNode input
          (Attribute "class" ("my-container show-hidden-on-hover" ++ if input.dragged then " hidden" else ""))
          links
      Color ->
        Bootstrap.colorToHtmlNode input containerClass links
      _ ->
        Bootstrap.textInputToHtmlNode input containerClass links

-------------
-- Private --
-------------

toLegend : Input -> Node
toLegend input =
  let
    i1 = i "" [Attribute "class" "fa fa-edit control-element"] [] []
    l1 = a "" [Attribute "href" ("#input/" ++ toString input.id)] [] [i1]

    l2 = iconLink "fa-trash" (Html.Events.onClick (Messages.FormMessage (Messages.RemoveInput input.id)))
    l3 = iconLink "fa-check" (Html.Events.onClick ((Messages.InputMessage (Messages.ToggleDisabled input.id))))
    l4 = iconLink "fa-arrows" (Html.Events.onMouseDown (Messages.MouseMessage (Messages.MouseClick input.id)))

    divider = span " " [] [] []

    children = [ l1, divider, l2, divider, l3, divider, l4 ]
    links = span "" [Attribute "class" "hidden-inherit float-right one-rem-size"] [] children
    label = span (Maybe.withDefault "" input.label) [] [] []
  in
    legend "" [] [] [label, links]

toLinks : Int -> Node
toLinks value =
  let
    divider = span " " [] [] []
    links =
      [ iconLink "fa-font fa-small" (onClick (Messages.InputMessage (Messages.SizeEdit value "small")))
      , iconLink "fa-font fa-normal" (onClick (Messages.InputMessage (Messages.SizeEdit value "normal")))
      , iconLink "fa-font fa-big" (onClick (Messages.InputMessage (Messages.SizeEdit value "large")))
      , a "" [Attribute "href" ("#input/" ++ toString value)] []
        [ i "" [Attribute "class" "fa fa-edit control-element"] [] [] ]
      , iconLink "fa-trash" (onClick (Messages.FormMessage (Messages.RemoveInput value)))
      , iconLink "fa-check" (onClick (Messages.InputMessage (Messages.ToggleDisabled value)))
      , iconLink "fa-arrows" (onMouseDown (Messages.MouseMessage (Messages.MouseClick value)))
      ] |> List.intersperse divider
  in
    div "" [Attribute "class" "control-container hidden-block"] [] links

iconLink : String -> (Html.Attribute Messages.Msg) -> Node
iconLink class event =
  let
    icon = i "" [Attribute "class" ("fa control-element " ++ class)] [] []
  in
    span "" [] [event] [icon]
