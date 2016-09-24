module Templates exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Messages exposing (..)

view =
  [ b [] [text "Text input"]
  , a [href "javascript:void(0);", onClick (FormMessage AddTextInput), class "pull-xs-right"] [text "add"]
  , hr [] []
  , textInput

  , b [] [text "Select"]
  , a [href "javascript:void(0);", onClick (FormMessage AddSelect), class "pull-xs-right"] [text "add"]
  , hr [] []
  , select

  , b [] [text "Multiselect"]
  , a [href "javascript:void(0);", onClick (FormMessage AddMultiselect), class "pull-xs-right"] [text "add"]
  , hr [] []
  , multiSelect

  , b [] [text "Textarea"]
  , a [href "javascript:void(0);", onClick (FormMessage AddTextarea), class "pull-xs-right"] [text "add"]
  , hr [] []
  , textarea

  , b [] [text "File upload"]
  , a [href "javascript:void(0);", onClick (FormMessage AddFileUpload), class "pull-xs-right"] [text "add"]
  , hr [] []
  , fileUpload

  , b [] [text "Radio buttons"]
  , a [href "javascript:void(0);", onClick (FormMessage AddRadioButtons), class "pull-xs-right"] [text "add"]
  , hr [] []
  , radioButtons

  , b [] [text "Checkbox"]
  , a [href "javascript:void(0);", onClick (FormMessage AddCheckbox), class "pull-xs-right"] [text "add"]
  , hr [] []
  , checkbox

  , b [] [text "Button"]
  , a [href "javascript:void(0);", onClick (FormMessage AddButton), class "pull-xs-right"] [text "add"]
  , hr [] []
  , submit
  ]

-------------
-- Private --
-------------

textInput =
  div
    [ class "form-group" ]
    [ input [class "form-control"] [] ]

select =
  div
    [ class "form-group" ]
    [ Html.select
      [class "form-control"]
      [ option [] [text "1"]
      , option [] [text "2"]
      , option [] [text "3"]
      ]
    ]

multiSelect =
  div
    [ class "form-group" ]
    [ Html.select
      [class "form-control", multiple True]
      [ option [] [text "1"]
      , option [] [text "2"]
      , option [] [text "3"]
      , option [] [text "4"]
      , option [] [text "5"]
      ]
    ]

textarea =
  div
    [ class "form-group" ]
    [ Html.textarea [class "form-control", rows 3] [] ]

fileUpload =
  div
    [ class "form-group" ]
    [ input [type' "file", class "form-control-file", Html.Attributes.attribute "aria-describedby" "fileHelp"] [] ]

radioButtons =
  fieldset
    [ class "form-group" ]
    [ div
        [ class "form-check"]
        [ label
          [ class "form-check-label"]
          [ input
            [type' "radio", class "form-check-input", name "optionsRadio", id "optionsRadio1", value "option1", checked True]
            []
          , text "Option 1"
          ]
        ]
      , div
        [ class "form-check"]
        [ label
          [ class "form-check-label"]
          [ input
            [type' "radio", class "form-check-input", name "optionsRadio", id "optionsRadio2", value "option2"]
            []
          , text "Option 2"
          ]
        ]
      , div
        [ class "form-check"]
        [ label
          [ class "form-check-label"]
          [ input
            [type' "radio", class "form-check-input", name "optionsRadio", id "optionsRadio3", value "option3"]
            []
          , text "Option 3"
          ]
        ]
    ]

checkbox =
  div
    [ class "form-check" ]
    [ label
        [ class "form-check-label"]
        [ input [type' "checkbox", class "form-check-input"] []
        , text "Check me out" ]
    ]

submit =
  div
    [class "form-group"]
    [ button [class "btn btn-primary"] [text "Submit button"] ]
