{-# LANGUAGE DataKinds #-}
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
  , addToRunLoop_forModeSelector
  , displayLinkWithTarget_selectorSelector
  , durationSelector
  , frameIntervalSelector
  , invalidateSelector
  , pausedSelector
  , preferredFrameRateRangeSelector
  , preferredFramesPerSecondSelector
  , removeFromRunLoop_forModeSelector
  , setFrameIntervalSelector
  , setPausedSelector
  , setPreferredFrameRateRangeSelector
  , setPreferredFramesPerSecondSelector
  , targetTimestampSelector
  , timestampSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.QuartzCore.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ displayLinkWithTarget:selector:@
displayLinkWithTarget_selector :: RawId -> Sel -> IO (Id CADisplayLink)
displayLinkWithTarget_selector target sel =
  do
    cls' <- getRequiredClass "CADisplayLink"
    sendClassMessage cls' displayLinkWithTarget_selectorSelector target sel

-- | @- addToRunLoop:forMode:@
addToRunLoop_forMode :: (IsCADisplayLink caDisplayLink, IsNSRunLoop runloop, IsNSString mode) => caDisplayLink -> runloop -> mode -> IO ()
addToRunLoop_forMode caDisplayLink runloop mode =
  sendMessage caDisplayLink addToRunLoop_forModeSelector (toNSRunLoop runloop) (toNSString mode)

-- | @- removeFromRunLoop:forMode:@
removeFromRunLoop_forMode :: (IsCADisplayLink caDisplayLink, IsNSRunLoop runloop, IsNSString mode) => caDisplayLink -> runloop -> mode -> IO ()
removeFromRunLoop_forMode caDisplayLink runloop mode =
  sendMessage caDisplayLink removeFromRunLoop_forModeSelector (toNSRunLoop runloop) (toNSString mode)

-- | @- invalidate@
invalidate :: IsCADisplayLink caDisplayLink => caDisplayLink -> IO ()
invalidate caDisplayLink =
  sendMessage caDisplayLink invalidateSelector

-- | @- timestamp@
timestamp :: IsCADisplayLink caDisplayLink => caDisplayLink -> IO CDouble
timestamp caDisplayLink =
  sendMessage caDisplayLink timestampSelector

-- | @- duration@
duration :: IsCADisplayLink caDisplayLink => caDisplayLink -> IO CDouble
duration caDisplayLink =
  sendMessage caDisplayLink durationSelector

-- | @- targetTimestamp@
targetTimestamp :: IsCADisplayLink caDisplayLink => caDisplayLink -> IO CDouble
targetTimestamp caDisplayLink =
  sendMessage caDisplayLink targetTimestampSelector

-- | @- paused@
paused :: IsCADisplayLink caDisplayLink => caDisplayLink -> IO Bool
paused caDisplayLink =
  sendMessage caDisplayLink pausedSelector

-- | @- setPaused:@
setPaused :: IsCADisplayLink caDisplayLink => caDisplayLink -> Bool -> IO ()
setPaused caDisplayLink value =
  sendMessage caDisplayLink setPausedSelector value

-- | @- frameInterval@
frameInterval :: IsCADisplayLink caDisplayLink => caDisplayLink -> IO CLong
frameInterval caDisplayLink =
  sendMessage caDisplayLink frameIntervalSelector

-- | @- setFrameInterval:@
setFrameInterval :: IsCADisplayLink caDisplayLink => caDisplayLink -> CLong -> IO ()
setFrameInterval caDisplayLink value =
  sendMessage caDisplayLink setFrameIntervalSelector value

-- | @- preferredFramesPerSecond@
preferredFramesPerSecond :: IsCADisplayLink caDisplayLink => caDisplayLink -> IO CLong
preferredFramesPerSecond caDisplayLink =
  sendMessage caDisplayLink preferredFramesPerSecondSelector

-- | @- setPreferredFramesPerSecond:@
setPreferredFramesPerSecond :: IsCADisplayLink caDisplayLink => caDisplayLink -> CLong -> IO ()
setPreferredFramesPerSecond caDisplayLink value =
  sendMessage caDisplayLink setPreferredFramesPerSecondSelector value

-- | @- preferredFrameRateRange@
preferredFrameRateRange :: IsCADisplayLink caDisplayLink => caDisplayLink -> IO CAFrameRateRange
preferredFrameRateRange caDisplayLink =
  sendMessage caDisplayLink preferredFrameRateRangeSelector

-- | @- setPreferredFrameRateRange:@
setPreferredFrameRateRange :: IsCADisplayLink caDisplayLink => caDisplayLink -> CAFrameRateRange -> IO ()
setPreferredFrameRateRange caDisplayLink value =
  sendMessage caDisplayLink setPreferredFrameRateRangeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @displayLinkWithTarget:selector:@
displayLinkWithTarget_selectorSelector :: Selector '[RawId, Sel] (Id CADisplayLink)
displayLinkWithTarget_selectorSelector = mkSelector "displayLinkWithTarget:selector:"

-- | @Selector@ for @addToRunLoop:forMode:@
addToRunLoop_forModeSelector :: Selector '[Id NSRunLoop, Id NSString] ()
addToRunLoop_forModeSelector = mkSelector "addToRunLoop:forMode:"

-- | @Selector@ for @removeFromRunLoop:forMode:@
removeFromRunLoop_forModeSelector :: Selector '[Id NSRunLoop, Id NSString] ()
removeFromRunLoop_forModeSelector = mkSelector "removeFromRunLoop:forMode:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector '[] ()
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector '[] CDouble
timestampSelector = mkSelector "timestamp"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @targetTimestamp@
targetTimestampSelector :: Selector '[] CDouble
targetTimestampSelector = mkSelector "targetTimestamp"

-- | @Selector@ for @paused@
pausedSelector :: Selector '[] Bool
pausedSelector = mkSelector "paused"

-- | @Selector@ for @setPaused:@
setPausedSelector :: Selector '[Bool] ()
setPausedSelector = mkSelector "setPaused:"

-- | @Selector@ for @frameInterval@
frameIntervalSelector :: Selector '[] CLong
frameIntervalSelector = mkSelector "frameInterval"

-- | @Selector@ for @setFrameInterval:@
setFrameIntervalSelector :: Selector '[CLong] ()
setFrameIntervalSelector = mkSelector "setFrameInterval:"

-- | @Selector@ for @preferredFramesPerSecond@
preferredFramesPerSecondSelector :: Selector '[] CLong
preferredFramesPerSecondSelector = mkSelector "preferredFramesPerSecond"

-- | @Selector@ for @setPreferredFramesPerSecond:@
setPreferredFramesPerSecondSelector :: Selector '[CLong] ()
setPreferredFramesPerSecondSelector = mkSelector "setPreferredFramesPerSecond:"

-- | @Selector@ for @preferredFrameRateRange@
preferredFrameRateRangeSelector :: Selector '[] CAFrameRateRange
preferredFrameRateRangeSelector = mkSelector "preferredFrameRateRange"

-- | @Selector@ for @setPreferredFrameRateRange:@
setPreferredFrameRateRangeSelector :: Selector '[CAFrameRateRange] ()
setPreferredFrameRateRangeSelector = mkSelector "setPreferredFrameRateRange:"

