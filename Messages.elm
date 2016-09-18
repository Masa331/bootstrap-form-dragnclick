module Messages exposing (..)

type Msg =
  AddTextInput
  | AddSelect
  | AddMultiselect
  | AddTextarea
  | AddFileUpload
  | AddRadioButtons
  | AddCheckbox
  | AddButton
  | RemoveInput Int
  | EditInput Int
