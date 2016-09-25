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
  | RemoveInput Int
  | EditInput Int
  | StopEditing

type InputMsg 
  = PlaceholderEdit String
  | NoOp

type Msg =
  FormMessage FormMsg
  | InputMessage InputMsg
