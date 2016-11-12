module WithControlElements exposing (build)
import String
import Html.Events
import Html.Attributes

import HtmlTree exposing (..)
import FormModel exposing (..)
import Messages
import Models

build : Input -> HtmlTree.Element
build input =
  case input.type' of
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
      , Just (toSmall inp)
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
      , toType inp.type'
      ] |> List.filterMap identity

    containerClass =
      [ Just "form-group"
      , if inp.dragged then Just "hidden" else Nothing
      ] |> List.filterMap identity
        |> String.join " "
        |> Attribute "class"

    children =
      [ toLabel inp.label
      , Just (Element "input" inputAttrs (Children []) "" [])
      , Just (toSmall inp)
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    Element "div" [containerClass, Attribute "data-input-id" (toString inp.id) ] (Children children) "" []

selectToHtmlTree inp =
  let
    inputAttrs =
      [ toId inp.id
      , toDisabled inp.disabled
      , toClasses ((sizeClass inp.size) :: [ "form-control" ])
      ] |> List.filterMap identity

    containerClass =
      [ Just "form-group"
      , if inp.dragged then Just "hidden" else Nothing
      ] |> List.filterMap identity
        |> String.join " "
        |> Attribute "class"

    options = List.map (\value -> Element "option" [] (Children []) value []) inp.options
    children =
      [ toLabel inp.label
      , Just (Element "select" inputAttrs (Children options) "" [])
      , Just (toSmall inp)
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    Element "div" [containerClass, Attribute "data-input-id" (toString inp.id) ] (Children children) "" []

textAreaToHtmlTree inp =
  let
    -- inputAttrs =
    --   [ toId inp.id
    --   , toPlaceholder inp.placeholder
    --   , toDisabled inp.disabled
    --   , toClasses [ "form-control" ]
    --   , Just (Attribute "rows" (inp.rowNumber))
    --   ] |> List.filterMap identity

    containerClass =
      [ Just "form-group"
      , if inp.dragged then Just "hidden" else Nothing
      ] |> List.filterMap identity
        |> String.join " "
        |> Attribute "class"

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
      , toClasses ((sizeClass inp.size) :: [ "form-control" ])
      , Just (Attribute "multiple" "multiple")
      ] |> List.filterMap identity

    containerClass =
      [ Just "form-group"
      , if inp.dragged then Just "hidden" else Nothing
      ] |> List.filterMap identity
        |> String.join " "
        |> Attribute "class"

    options = List.map (\value -> Element "option" [] (Children []) value []) inp.options
    children =
      [ toLabel inp.label
      , Just (Element "select" inputAttrs (Children options) "" [])
      , Just (toSmall inp)
      , toLinks inp.id
      ] |> List.filterMap identity
  in
    Element "div" [containerClass, Attribute "data-input-id" (toString inp.id) ] (Children children) "" []

fileUploadToHtmlTree inp =
  let
    inputAttrs =
      [ toId inp.id
      , toDisabled inp.disabled
      , toClasses ((sizeClass inp.size) :: inp.classList)
      , Just (Attribute "type" "file")
      ] |> List.filterMap identity

    containerClass =
      [ Just "form-group"
      , if inp.dragged then Just "hidden" else Nothing
      ] |> List.filterMap identity
        |> String.join " "
        |> Attribute "class"

    children =
      [ toLabel inp.label
      , Just (Element "input" inputAttrs (Children []) "" [])
      , Just (toSmall inp)
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
      ++ [ Just (toSmall inp) ]
      ++ [ toLinks inp.id ]
      |> List.filterMap identity

    containerClass =
      [ Just "form-group"
      , if inp.dragged then Just "hidden" else Nothing
      ] |> List.filterMap identity
        |> String.join " "
        |> Attribute "class"
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

    containerClass =
      [ Just "form-check"
      , if inp.dragged then Just "hidden" else Nothing
      ] |> List.filterMap identity
        |> String.join " "
        |> Attribute "class"
  in
    Element "div" [containerClass, Attribute "data-input-id" (toString inp.id) ] (Children children) "" []

buttonToHtmlTree inp =
  let
    children =
      [ Just (Element "button" [Attribute "type" "submit", Attribute "class" "btn btn-primary"] (Children []) (Maybe.withDefault "Submit" inp.label) [])
      , toLinks inp.id
      ] |> List.filterMap identity

    containerClass =
      [ Just "my-container"
      , if inp.dragged then Just "hidden" else Nothing
      ] |> List.filterMap identity
        |> String.join " "
        |> Attribute "class"
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

toAddon : Maybe String -> Element
toAddon value =
  let
    editLink = Element "small" [Attribute "class" "control-element hidden-inline-block"] (Children []) "Edit" []
  in
    case value of
      Just text ->
        Element "div" [Attribute "class" "input-group-addon"] (Children [Element "span" [] (Children []) text [], editLink]) "" []
      Nothing ->
        Element "div" [Attribute "class" "input-group-addon hidden-table-cell"] (Children [editLink]) "" []

toSmall : Input -> Element
toSmall input =
  case input.small of
    Just text ->
      -- Element "small" [Attribute "class" "form-text text-muted"] (Children []) text []
      let
        smallText = Element "span" [Attribute "class" "text-muted"] (Children []) text []
        editLink = Element "span" [Attribute "class" "control-element hidden-inline-block"] (Children []) " Edit" []
      in
        Element "small" [Attribute "class" "form-text"] (Children [smallText, editLink]) "" []
    Nothing ->
      let
        editLink = Element "small" [Attribute "class" "form-text control-element no-margin"] (Children []) "Click here to edit small text under." []
      in
        Element "div" [Attribute "class" "hidden-block absolute-position"] (Children [editLink]) "" []

toLabel : Maybe String -> Maybe Element
toLabel value =
  let
    innerSpan = Element "span" [] (Children []) (Maybe.withDefault "" value) []
    editControl = Element "small" [ Attribute "class" "control-element hidden-inline-block" ] (Children []) " Edit" []
  in
    Just (Element "label" [Attribute "for" "input1"] (Children [innerSpan, editControl]) "" [])

toLegend : Maybe String -> Maybe Element
toLegend value =
  Maybe.map (\value -> Element "legend" [] (Children []) value []) value

toType : InputType -> Maybe Attribute
toType value =
  Just (Attribute "type" (inputTypeToString value))

toLinks : Id -> Maybe Element
toLinks value =
  let
    i1 = Element "i" [Attribute "class" "fa fa-font fa-small control-element"] (Children []) "" []
    l1 = Element "span" [] (Children [i1]) "" [Html.Events.onClick ((Messages.InputMessage (Messages.SizeEdit value "small")))]
    i2 = Element "i" [Attribute "class" "fa fa-font fa-normal control-element"] (Children []) "" []
    l2 = Element "span" [] (Children [i2]) "" [Html.Events.onClick ((Messages.InputMessage (Messages.SizeEdit value "normal")))]
    i3 = Element "i" [Attribute "class" "fa fa-font fa-big control-element"] (Children []) "" []
    l3 = Element "span" [] (Children [i3]) "" [Html.Events.onClick ((Messages.InputMessage (Messages.SizeEdit value "large")))]

    i4 = Element "i" [Attribute "class" "fa fa-edit control-element"] (Children []) "" []
    l4 = Element "span" [] (Children [i4]) "" [Html.Events.onClick (Messages.FormMessage (Messages.EditInput value))]
    i5 = Element "i" [Attribute "class" "fa fa-trash control-element"] (Children []) "" []
    l5 = Element "span" [] (Children [i5]) "" [Html.Events.onClick (Messages.FormMessage (Messages.RemoveInput value))]
    i6 = Element "i" [Attribute "class" "fa fa-check control-element"] (Children []) "" []
    l6 = Element "span" [] (Children [i6]) "" [Html.Events.onClick ((Messages.InputMessage (Messages.ToggleDisabled value)))]
    i7 = Element "i" [Attribute "class" "fa fa-arrows control-element"] (Children []) "" []
    l7 = Element "span" [] (Children [i7]) "" [Html.Events.onMouseDown ((Messages.MouseMessage (Messages.MouseClick value)))]

    children = (Children [l1, l2, l3, l4, l5, l6, l7])
  in
    Just (Element "div" [Attribute "class" "control-container hidden-block"] children "" [])

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

wrapInAddons input =
  let
    inputAttrs =
      [ toId input.id
      , toPlaceholder input.placeholder
      , toDisabled input.disabled
      , toClasses ((sizeClass input.size) :: ([ "form-control" ] ++ input.classList))
      , toType input.type'
      ] |> List.filterMap identity

    add1 = toAddon input.addon1
    add2 = toAddon input.addon2
    inputType =
      case input.type' of
        TextArea -> "textarea"
        _ -> "input"

    input1 = Element inputType inputAttrs (Children []) "" []
    inputClasses =
      case input.size of
        Small ->
          "input-group input-group-sm"
        Normal ->
          "input-group"
        Large ->
          "input-group input-group-lg"
  in
    Just (Element "div" [Attribute "class" inputClasses] (Children ([add1, input1, add2])) "" [])
