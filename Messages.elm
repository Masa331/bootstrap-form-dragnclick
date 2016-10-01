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
  | SmallEdit String
  | DisabledEdit Bool
  | ReadonlyEdit Bool
  | FirstAddonEdit String
  | SecondAddonEdit String
  | SizeEdit String
  | TypeEdit String

type Msg =
  FormMessage FormMsg
  | InputMessage InputMsg
