module Graphics.UI.SDL.Timer.Internal
       ( unsafeSetLastTicks
       , getAbsTicks
       ) where

import Data.IORef
import Data.Word
import Data.Int
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (when)

import Graphics.UI.SDL.Timer.Types (Ticks)

-- Here we use yet another global variable to convert SDL's machine ticks into
-- variable length tick number.
-- We use IORef because there are no possible inconsistencies -- last write is
-- right regardless of operations order.
-- Int32 is used for safe difference calculation.
lastTicks :: IORef (Ticks, Int32)
{-# NOINLINE lastTicks #-}
lastTicks = unsafePerformIO $ newIORef undefined

-- This is not safe (because it breaks the "last write is always right" rule),
-- so this is used in one place only -- at the SDL init.
unsafeSetLastTicks :: Ticks -> IO ()
-- fromIntegral t :: Word32 is the same as taking "mod"
unsafeSetLastTicks t
  | t >= 0 = writeIORef lastTicks (t, fromIntegral t)
  | otherwise = fail "setLastTicks: number of ticks should be positive"

getAbsTicks :: Word32 -> IO Ticks
getAbsTicks nv' = do
  (t, v) <- readIORef lastTicks
  let nv = fromIntegral nv'
      nt = t + fromIntegral (nv - v)
  when (nt < 0) $ fail "getTicks: tick difference is negative"
  writeIORef lastTicks (nt, nv)
  return nt
