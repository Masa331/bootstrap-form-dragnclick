module InputToHtmlTreeConverter exposing (..)

import HtmlTree exposing (..)
import FormModel exposing (..)

import String exposing (join)

inputToHtmlTree input =
  case input of
    TextInput attrs -> textInputToHtmlTree input attrs
    TextArea attrs -> textAreaToHtmlTree input attrs
    Select attrs -> selectToHtmlTree input attrs
    Multiselect attrs -> multiselectToHtmlTree input attrs
    FileUpload attrs -> fileUploadToHtmlTree input attrs
    Radio attrs -> radioToHtmlTree input attrs
    Checkbox attrs -> checkboxToHtmlTree input attrs
    Button attrs -> buttonToHtmlTree input attrs

textInputToHtmlTree inp attrs =
  let
    id = toId attrs.id
    placeholder = toPlaceholder attrs.placeholder
    disabled = toDisabled attrs.disabled
    readonly = toReadonly attrs.readonly
    inputClasses = toClasses attrs.classList attrs.size
    inputType = toType attrs.type'
    inputAttrs =
      [ id, inputClasses, placeholder, inputType, readonly, disabled ]
      |> List.filterMap identity

    inputLabel = toLabel attrs.label
    smallText = toSmall attrs.small
    links = toLinks (extractId inp)
    add1 = toAddon attrs.addon1
    add2 = toAddon attrs.addon2
    input1 = Just (Element "input" inputAttrs (Children []) "")
    input =
      if List.isEmpty (List.filterMap identity [add1, add2]) then
        input1
      else
        Just (Element "div" [Attribute "class" "input-group"] (Children ([add1, input1, add2] |> List.filterMap identity)) "")

    children = [ inputLabel, input, smallText, links ] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-group"] (Children children) ""

selectToHtmlTree inp attrs =
  let
    id = toId attrs.id
    disabled = toDisabled attrs.disabled
    inputClasses = toClasses attrs.classList attrs.size
    selectAttrs = [ id, inputClasses, disabled ] |> List.filterMap identity

    options = List.map (\value -> Element "option" [] (Children []) value) attrs.options
    inputLabel = toLabel attrs.label
    smallText = toSmall attrs.small
    links = toLinks (extractId inp)
    select = Just (Element "select" selectAttrs (Children options) "")
    children = [inputLabel, select, smallText, links] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-group"] (Children children) ""

textAreaToHtmlTree inp attrs =
  let
    inputLabel = toLabel attrs.label
    id = toId attrs.id
    disabled = toDisabled attrs.disabled
    placeholder = toPlaceholder attrs.placeholder
    inputClasses = toClasses attrs.classList Normal
    rowNumber = Just (Attribute "rows" (toString attrs.rowNumber))
    links = toLinks (extractId inp)

    areaAttrs = [ id, inputClasses, disabled, placeholder, rowNumber ] |> List.filterMap identity
    area = Just (Element "textarea" areaAttrs (Children []) "")
  in
    Element "div" [Attribute "class" "form-group"] (Children ([inputLabel, area, links] |> List.filterMap identity)) ""

multiselectToHtmlTree inp attrs =
  let
    id = toId attrs.id
    disabled = toDisabled attrs.disabled
    inputClasses = toClasses attrs.classList Normal
    multiple = Just (Attribute "multiple" "multiple")
    selectAttrs = [ id, inputClasses, disabled, multiple ] |> List.filterMap identity

    options = List.map (\value -> Element "option" [] (Children []) value) attrs.options
    inputLabel = toLabel attrs.label
    smallText = toSmall attrs.small
    links = toLinks (extractId inp)
    select = Just (Element "select" selectAttrs (Children options) "")
    children = [inputLabel, select, smallText, links] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-group"] (Children children) ""

fileUploadToHtmlTree inp attrs =
  let
    id = toId attrs.id
    disabled = toDisabled attrs.disabled
    inputClasses = toClasses attrs.classList Normal
    uploadType = Just (Attribute "type" "file")
    uploadAttrs = [ id, inputClasses, disabled, uploadType ] |> List.filterMap identity

    inputLabel = toLabel attrs.label
    smallText = toSmall attrs.small
    links = toLinks (extractId inp)
    upload = Just (Element "input" uploadAttrs (Children []) "")

    children = [inputLabel, upload, smallText, links] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-group"] (Children children) ""

radioToHtmlTree inp attrs =
  let
    inputAttrs = [Attribute "type" "text", Attribute "class" "form-control", Attribute "id" "input1"]
    input = Element "input" inputAttrs (Children []) ""

    options = List.map (\value -> Just (toRadioOption attrs.id 1 value)) attrs.options
    legend = toLegend attrs.label

    links = toLinks (extractId inp)
    children = ([legend] ++ options ++ [links]) |> List.filterMap identity
  in
    Element "fieldset" [Attribute "class" "form-group"] (Children children ) ""

toRadioOption id index value =
  let
    input = Element "input" [Attribute "type" "radio", Attribute "class" "form-check-input", Attribute "name" (toString id), Attribute "id" (toString id), Attribute "value" value] (Children []) ""
    children = Element "label" [Attribute "class" "form-check-label"] (Children [input]) value
  in
    Element "div" [Attribute "class" "form-check"] (Children [children]) ""


checkboxToHtmlTree inp attrs =
  let
    input = Element "input" [Attribute "type" "checkbox", Attribute "class" "form-check-input"] (Children []) ""
    label =
      case attrs.label of
        Nothing ->
          Just (Element "label" [Attribute "class" "form-check-label"] (Children [input]) "")
        Just value ->
          Just (Element "label" [Attribute "class" "form-check-label"] (Children [input]) value)

    links = toLinks (extractId inp)
    children = [label, links] |> List.filterMap identity
  in
    Element "div" [Attribute "class" "form-check"] (Children children) ""

buttonToHtmlTree inp attrs =
  let
    button = Just (Element "button" [Attribute "type" "submit", Attribute "class" "btn btn-primary"] (Children []) (Maybe.withDefault "Submit" attrs.label))
    links = toLinks (extractId inp)
  in
    Element "div" [Attribute "class" "my-container"] (Children ([button, links] |> List.filterMap identity)) ""


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
