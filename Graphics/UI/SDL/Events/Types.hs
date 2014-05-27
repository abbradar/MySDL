module Graphics.UI.SDL.Events.Types (
  Timestamp,
  WindowID,
  Event(..)
  ) where

import Data.Word
import Graphics.UI.SDL.Video.Events

type Timestamp = Word32
type WindowID = Word32

data Event = Window { weTimestamp :: !Timestamp,
                      windowID :: !WindowID,
                      event :: !WindowEvent }
           | Keyboard { kbTimestamp :: !Timestamp }
