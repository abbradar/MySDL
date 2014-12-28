{-|
Description: SDL video subsystem.
-}

module Graphics.UI.SDL.Video
       ( module Keyboard
       , module KeyboardTypes
       , module OpenGL
       , module ScreenSaver
       , module Mouse
       , module Surface
       , module WMInfo
       , module Window
       ) where

import Graphics.UI.SDL.Video.Keyboard as Keyboard
import Graphics.UI.SDL.Video.Keyboard.Types as KeyboardTypes
import Graphics.UI.SDL.Video.OpenGL as OpenGL
import Graphics.UI.SDL.Video.ScreenSaver as ScreenSaver
import Graphics.UI.SDL.Video.Mouse as Mouse
import Graphics.UI.SDL.Video.Surface as Surface
import Graphics.UI.SDL.Video.WMInfo as WMInfo
import Graphics.UI.SDL.Video.Window as Window
