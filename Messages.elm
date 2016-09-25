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
  | Something
  -- RemoveInput Int
  -- | EditInput (Int)
  -- EditInput (Int)

type Msg =
  FormMessage FormMsg
  | InputMessage InputMsg
