{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Class representing a timer bound to the display vsync. *
--
-- Generated bindings for @CADisplayLink@.
module ObjC.QuartzCore.CADisplayLink
  ( CADisplayLink
  , IsCADisplayLink(..)
  , displayLinkWithTarget_selector
  , addToRunLoop_forMode
  , removeFromRunLoop_forMode
  , invalidate
  , timestamp
  , duration
  , targetTimestamp
  , paused
  , setPaused
  , frameInterval
  , setFrameInterval
  , preferredFramesPerSecond
  , setPreferredFramesPerSecond
  , preferredFrameRateRange
  , setPreferredFrameRateRange
  , displayLinkWithTarget_selectorSelector
  , addToRunLoop_forModeSelector
  , removeFromRunLoop_forModeSelector
  , invalidateSelector
  , timestampSelector
  , durationSelector
  , targetTimestampSelector
  , pausedSelector
  , setPausedSelector
  , frameIntervalSelector
  , setFrameIntervalSelector
  , preferredFramesPerSecondSelector
  , setPreferredFramesPerSecondSelector
  , preferredFrameRateRangeSelector
  , setPreferredFrameRateRangeSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.QuartzCore.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ displayLinkWithTarget:selector:@
displayLinkWithTarget_selector :: RawId -> Selector -> IO (Id CADisplayLink)
displayLinkWithTarget_selector target sel =
  do
    cls' <- getRequiredClass "CADisplayLink"
    sendClassMsg cls' (mkSelector "displayLinkWithTarget:selector:") (retPtr retVoid) [argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector sel)] >>= retainedObject . castPtr

-- | @- addToRunLoop:forMode:@
addToRunLoop_forMode :: (IsCADisplayLink caDisplayLink, IsNSRunLoop runloop, IsNSString mode) => caDisplayLink -> runloop -> mode -> IO ()
addToRunLoop_forMode caDisplayLink  runloop mode =
withObjCPtr runloop $ \raw_runloop ->
  withObjCPtr mode $ \raw_mode ->
      sendMsg caDisplayLink (mkSelector "addToRunLoop:forMode:") retVoid [argPtr (castPtr raw_runloop :: Ptr ()), argPtr (castPtr raw_mode :: Ptr ())]

-- | @- removeFromRunLoop:forMode:@
removeFromRunLoop_forMode :: (IsCADisplayLink caDisplayLink, IsNSRunLoop runloop, IsNSString mode) => caDisplayLink -> runloop -> mode -> IO ()
removeFromRunLoop_forMode caDisplayLink  runloop mode =
withObjCPtr runloop $ \raw_runloop ->
  withObjCPtr mode $ \raw_mode ->
      sendMsg caDisplayLink (mkSelector "removeFromRunLoop:forMode:") retVoid [argPtr (castPtr raw_runloop :: Ptr ()), argPtr (castPtr raw_mode :: Ptr ())]

-- | @- invalidate@
invalidate :: IsCADisplayLink caDisplayLink => caDisplayLink -> IO ()
invalidate caDisplayLink  =
  sendMsg caDisplayLink (mkSelector "invalidate") retVoid []

-- | @- timestamp@
timestamp :: IsCADisplayLink caDisplayLink => caDisplayLink -> IO CDouble
timestamp caDisplayLink  =
  sendMsg caDisplayLink (mkSelector "timestamp") retCDouble []

-- | @- duration@
duration :: IsCADisplayLink caDisplayLink => caDisplayLink -> IO CDouble
duration caDisplayLink  =
  sendMsg caDisplayLink (mkSelector "duration") retCDouble []

-- | @- targetTimestamp@
targetTimestamp :: IsCADisplayLink caDisplayLink => caDisplayLink -> IO CDouble
targetTimestamp caDisplayLink  =
  sendMsg caDisplayLink (mkSelector "targetTimestamp") retCDouble []

-- | @- paused@
paused :: IsCADisplayLink caDisplayLink => caDisplayLink -> IO Bool
paused caDisplayLink  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg caDisplayLink (mkSelector "paused") retCULong []

-- | @- setPaused:@
setPaused :: IsCADisplayLink caDisplayLink => caDisplayLink -> Bool -> IO ()
setPaused caDisplayLink  value =
  sendMsg caDisplayLink (mkSelector "setPaused:") retVoid [argCULong (if value then 1 else 0)]

-- | @- frameInterval@
frameInterval :: IsCADisplayLink caDisplayLink => caDisplayLink -> IO CLong
frameInterval caDisplayLink  =
  sendMsg caDisplayLink (mkSelector "frameInterval") retCLong []

-- | @- setFrameInterval:@
setFrameInterval :: IsCADisplayLink caDisplayLink => caDisplayLink -> CLong -> IO ()
setFrameInterval caDisplayLink  value =
  sendMsg caDisplayLink (mkSelector "setFrameInterval:") retVoid [argCLong (fromIntegral value)]

-- | @- preferredFramesPerSecond@
preferredFramesPerSecond :: IsCADisplayLink caDisplayLink => caDisplayLink -> IO CLong
preferredFramesPerSecond caDisplayLink  =
  sendMsg caDisplayLink (mkSelector "preferredFramesPerSecond") retCLong []

-- | @- setPreferredFramesPerSecond:@
setPreferredFramesPerSecond :: IsCADisplayLink caDisplayLink => caDisplayLink -> CLong -> IO ()
setPreferredFramesPerSecond caDisplayLink  value =
  sendMsg caDisplayLink (mkSelector "setPreferredFramesPerSecond:") retVoid [argCLong (fromIntegral value)]

-- | @- preferredFrameRateRange@
preferredFrameRateRange :: IsCADisplayLink caDisplayLink => caDisplayLink -> IO CAFrameRateRange
preferredFrameRateRange caDisplayLink  =
  sendMsgStret caDisplayLink (mkSelector "preferredFrameRateRange") retCAFrameRateRange []

-- | @- setPreferredFrameRateRange:@
setPreferredFrameRateRange :: IsCADisplayLink caDisplayLink => caDisplayLink -> CAFrameRateRange -> IO ()
setPreferredFrameRateRange caDisplayLink  value =
  sendMsg caDisplayLink (mkSelector "setPreferredFrameRateRange:") retVoid [argCAFrameRateRange value]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @displayLinkWithTarget:selector:@
displayLinkWithTarget_selectorSelector :: Selector
displayLinkWithTarget_selectorSelector = mkSelector "displayLinkWithTarget:selector:"

-- | @Selector@ for @addToRunLoop:forMode:@
addToRunLoop_forModeSelector :: Selector
addToRunLoop_forModeSelector = mkSelector "addToRunLoop:forMode:"

-- | @Selector@ for @removeFromRunLoop:forMode:@
removeFromRunLoop_forModeSelector :: Selector
removeFromRunLoop_forModeSelector = mkSelector "removeFromRunLoop:forMode:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector
timestampSelector = mkSelector "timestamp"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @targetTimestamp@
targetTimestampSelector :: Selector
targetTimestampSelector = mkSelector "targetTimestamp"

-- | @Selector@ for @paused@
pausedSelector :: Selector
pausedSelector = mkSelector "paused"

-- | @Selector@ for @setPaused:@
setPausedSelector :: Selector
setPausedSelector = mkSelector "setPaused:"

-- | @Selector@ for @frameInterval@
frameIntervalSelector :: Selector
frameIntervalSelector = mkSelector "frameInterval"

-- | @Selector@ for @setFrameInterval:@
setFrameIntervalSelector :: Selector
setFrameIntervalSelector = mkSelector "setFrameInterval:"

-- | @Selector@ for @preferredFramesPerSecond@
preferredFramesPerSecondSelector :: Selector
preferredFramesPerSecondSelector = mkSelector "preferredFramesPerSecond"

-- | @Selector@ for @setPreferredFramesPerSecond:@
setPreferredFramesPerSecondSelector :: Selector
setPreferredFramesPerSecondSelector = mkSelector "setPreferredFramesPerSecond:"

-- | @Selector@ for @preferredFrameRateRange@
preferredFrameRateRangeSelector :: Selector
preferredFrameRateRangeSelector = mkSelector "preferredFrameRateRange"

-- | @Selector@ for @setPreferredFrameRateRange:@
setPreferredFrameRateRangeSelector :: Selector
setPreferredFrameRateRangeSelector = mkSelector "setPreferredFrameRateRange:"

