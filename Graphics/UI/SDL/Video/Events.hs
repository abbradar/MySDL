module Graphics.UI.SDL.Video.Events (
  WindowEvent(..)
  ) where

import Data.Word

data WindowEvent = Shown
                 | Hidden
                 | Exposed
                 | Moved !Word32 !Word32
                 | Resized !Word32 !Word32
                 | Minimized
                 | Maximized
                 | Restored
                 | Enter
                 | Leave
                 | Gained
                 | FocusLost
                 | Close
