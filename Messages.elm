module Messages exposing (..)

type FormMsg =
  AddTextInput
  | AddSelect
  | AddMultiselect
  | AddTextarea
  | AddFileUpload
  | AddRadioButtons
  | AddCheckbox
  | AddButton

type InputMsg =
  RemoveInput Int
  | EditInput Int
  | StopEditing

type Msg =
  FormMessage FormMsg
  | InputMessage InputMsg
