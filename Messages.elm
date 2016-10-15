module Messages exposing (..)

import Mouse
import ElementMap

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
  | MoveUp Int
  | MoveDown Int

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
  | RowNumberEdit String
  | NewOptionEdit String
  | SaveNewOption
  | RemoveOption String

type MouseMsg
  = MouseDown Int
  | MouseUp Mouse.Position
  | MouseMove Mouse.Position

type Msg =
  FormMessage FormMsg
  | InputMessage InputMsg
  | MouseMessage MouseMsg
  | MapDetermined ElementMap.ElementMap
