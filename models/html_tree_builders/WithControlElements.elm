module WithControlElements exposing (build)

import Html.Events
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

    i2 = i "" [Attribute "class" "fa fa-trash control-element"] [] []
    l2 = span "" [] [Html.Events.onClick (Messages.FormMessage (Messages.RemoveInput input.id))] [i2]
    i3 = i "" [Attribute "class" "fa fa-check control-element"] [] []
    l3 = span "" [] [Html.Events.onClick ((Messages.InputMessage (Messages.ToggleDisabled input.id)))] [i3]
    i4 = i "" [Attribute "class" "fa fa-arrows control-element"] [] []
    l4 = span "" [] [Html.Events.onMouseDown ((Messages.MouseMessage (Messages.MouseClick input.id)))] [i4]
    divider = span " " [] [] []

    children = [ l1, divider, l2, divider, l3, divider, l4 ]
    links = span "" [Attribute "class" "hidden-inherit float-right one-rem-size"] [] children
    label = span (Maybe.withDefault "" input.label) [] [] []
  in
    legend "" [] [] [label, links]

toLinks : Int -> Maybe Node
toLinks value =
  let
    i1 = i "" [Attribute "class" "fa fa-font fa-small control-element"] [] []
    l1 = span "" [] [Html.Events.onClick ((Messages.InputMessage (Messages.SizeEdit value "small")))] [i1]
    i2 = i "" [Attribute "class" "fa fa-font fa-normal control-element"] [] []
    l2 = span "" [] [Html.Events.onClick ((Messages.InputMessage (Messages.SizeEdit value "normal")))] [i2]
    i3 = i "" [Attribute "class" "fa fa-font fa-big control-element"] [] []
    l3 = span "" [] [Html.Events.onClick ((Messages.InputMessage (Messages.SizeEdit value "large")))] [i3]

    i4 = i "" [Attribute "class" "fa fa-edit control-element"] [] []
    l4 = a "" [Attribute "href" ("#input/" ++ toString value)] [] [i4]

    i5 = i "" [Attribute "class" "fa fa-trash control-element"] [] []
    l5 = span "" [] [Html.Events.onClick (Messages.FormMessage (Messages.RemoveInput value))] [i5]
    i6 = i "" [Attribute "class" "fa fa-check control-element"] [] []
    l6 = span "" [] [Html.Events.onClick ((Messages.InputMessage (Messages.ToggleDisabled value)))] [i6]
    i7 = i "" [Attribute "class" "fa fa-arrows control-element"] [] []
    l7 = span "" [] [Html.Events.onMouseDown ((Messages.MouseMessage (Messages.MouseClick value)))] [i7]

    divider = span " " [] [] []

    children = [l1, divider, l2, divider, l3, divider, l4, divider, l5, divider, l6, divider, l7]
  in
    Just (div "" [Attribute "class" "control-container hidden-block"] [] children)
