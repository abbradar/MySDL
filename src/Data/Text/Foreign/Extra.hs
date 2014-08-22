module Data.Text.Foreign.Extra where

import Data.Text (Text)
import Data.Text.Foreign (peekCStringLen)
import Foreign.Marshal.Array (lengthArray0)
import Foreign.C.String (CString)

peekCString :: CString -> IO Text
peekCString s = do
  l <- lengthArray0 0 s
  peekCStringLen (s, l)
