{-# LANGUAGE RecordWildCards #-}

module Graphics.UI.SDL.Video.Internal.Keyboard
       ( CKeySym
       , fromCKeySym
       ) where

import Control.Applicative ((<$>))
import Foreign.C.Types (CInt(..), CUShort(..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)
import Data.Bits ((.&.))
import qualified Data.BitSet.Word as B

import Data.Enum.Num

{#import Graphics.UI.SDL.Video.Keyboard #}

#include <SDL2/SDL_keyboard.h>

{#pointer *SDL_Keysym as CKeySym #}

{#enum SDL_Keymod as CKeyMod {underscoreToCase} deriving (Eq, Show) #}

fromCKeySym :: CKeySym -> IO KeySym
fromCKeySym ks = do
  scanCode <- toEnum' <$> {#get SDL_Keysym->scancode #} ks
  keyCode <- toEnum' <$> {#get SDL_Keysym->sym #} ks
  bf <- {#get SDL_Keysym->mod #} ks

  let mods = [ (KmodLshift, ModLShift)
             , (KmodRshift, ModRShift)
             , (KmodLctrl, ModLCtrl)
             , (KmodRctrl, ModRCtrl)
             , (KmodLalt, ModLAlt)
             , (KmodRalt, ModRAlt)
             , (KmodLgui, ModLGUI)
             , (KmodRgui, ModRGUI)
             , (KmodNum, ModNum)
             , (KmodCaps, ModCaps)
             , (KmodMode, ModMode)
             ]
      keyMod = B.fromList $ map snd $ filter (\(m, _) -> bf .&. m /= 0) $ map (\(a, b) -> (fromEnum' a, b)) mods

  return KeySym { .. }
