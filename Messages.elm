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
  = PlaceholderEdit Int String
  | LabelEdit Int String
  | SmallEdit Int String
  | DisabledEdit Int Bool
  | ReadonlyEdit Int Bool
  | FirstAddonEdit Int String
  | SecondAddonEdit Int String
  | SizeEdit Int String
  | TypeEdit Int String
  | RowNumberEdit Int String
  | NewOptionEdit String
  | SaveNewOption Int
  | RemoveOption Int String

type MouseMsg
  = MouseDown Int
  | MouseUp Mouse.Position
  | MouseMove Mouse.Position

type Msg =
  FormMessage FormMsg
  | InputMessage InputMsg
  | MouseMessage MouseMsg
  | MapDetermined ElementMap.ElementMap
