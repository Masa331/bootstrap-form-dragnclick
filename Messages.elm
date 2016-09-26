module Messages exposing (..)

type FormMsg =
  AddTextInput
  | AddSelect
  | AddMultiselect
  | AddTextarea
  | AddFileUpload
  | AddRadio
  | AddCheckbox
  | AddButton
  | RemoveInput Int
  | EditInput Int
  | StopEditing

type InputMsg 
  = PlaceholderEdit String
  | LabelEdit String
  | NoOp

type Msg =
  FormMessage FormMsg
  | InputMessage InputMsg
