module Messages exposing (..)

import Mouse
import ElementMap
import Navigation

type FormMsg =
  AddInput
  | RemoveInput Int

type InputMsg
  = PlaceholderEdit Int String
  | LabelEdit Int String
  | SmallEdit Int String
  | ToggleDisabled Int
  | FirstAddonEdit Int String
  | SecondAddonEdit Int String
  | SizeEdit Int String
  | TypeEdit Int String
  | RowNumberEdit Int String
  | SaveNewOption Int
  | RemoveOption Int String

type MouseMsg
  = MouseClick Int
  | MouseUp Mouse.Position
  | MouseMove Mouse.Position

type Msg =
  FormMessage FormMsg
  | InputMessage InputMsg
  | MouseMessage MouseMsg
  | MapDetermined ElementMap.ElementMap
  | UrlChange Navigation.Location
  | NewOptionEdit String
