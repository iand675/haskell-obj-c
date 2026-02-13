{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CAMetalDisplayLink@.
module ObjC.QuartzCore.CAMetalDisplayLink
  ( CAMetalDisplayLink
  , IsCAMetalDisplayLink(..)
  , initWithMetalLayer
  , addToRunLoop_forMode
  , removeFromRunLoop_forMode
  , invalidate
  , delegate
  , setDelegate
  , preferredFrameLatency
  , setPreferredFrameLatency
  , preferredFrameRateRange
  , setPreferredFrameRateRange
  , paused
  , setPaused
  , addToRunLoop_forModeSelector
  , delegateSelector
  , initWithMetalLayerSelector
  , invalidateSelector
  , pausedSelector
  , preferredFrameLatencySelector
  , preferredFrameRateRangeSelector
  , removeFromRunLoop_forModeSelector
  , setDelegateSelector
  , setPausedSelector
  , setPreferredFrameLatencySelector
  , setPreferredFrameRateRangeSelector


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

-- | @- initWithMetalLayer:@
initWithMetalLayer :: (IsCAMetalDisplayLink caMetalDisplayLink, IsCAMetalLayer layer) => caMetalDisplayLink -> layer -> IO (Id CAMetalDisplayLink)
initWithMetalLayer caMetalDisplayLink layer =
  sendOwnedMessage caMetalDisplayLink initWithMetalLayerSelector (toCAMetalLayer layer)

-- | @- addToRunLoop:forMode:@
addToRunLoop_forMode :: (IsCAMetalDisplayLink caMetalDisplayLink, IsNSRunLoop runloop, IsNSString mode) => caMetalDisplayLink -> runloop -> mode -> IO ()
addToRunLoop_forMode caMetalDisplayLink runloop mode =
  sendMessage caMetalDisplayLink addToRunLoop_forModeSelector (toNSRunLoop runloop) (toNSString mode)

-- | @- removeFromRunLoop:forMode:@
removeFromRunLoop_forMode :: (IsCAMetalDisplayLink caMetalDisplayLink, IsNSRunLoop runloop, IsNSString mode) => caMetalDisplayLink -> runloop -> mode -> IO ()
removeFromRunLoop_forMode caMetalDisplayLink runloop mode =
  sendMessage caMetalDisplayLink removeFromRunLoop_forModeSelector (toNSRunLoop runloop) (toNSString mode)

-- | @- invalidate@
invalidate :: IsCAMetalDisplayLink caMetalDisplayLink => caMetalDisplayLink -> IO ()
invalidate caMetalDisplayLink =
  sendMessage caMetalDisplayLink invalidateSelector

-- | @- delegate@
delegate :: IsCAMetalDisplayLink caMetalDisplayLink => caMetalDisplayLink -> IO RawId
delegate caMetalDisplayLink =
  sendMessage caMetalDisplayLink delegateSelector

-- | @- setDelegate:@
setDelegate :: IsCAMetalDisplayLink caMetalDisplayLink => caMetalDisplayLink -> RawId -> IO ()
setDelegate caMetalDisplayLink value =
  sendMessage caMetalDisplayLink setDelegateSelector value

-- | @- preferredFrameLatency@
preferredFrameLatency :: IsCAMetalDisplayLink caMetalDisplayLink => caMetalDisplayLink -> IO CFloat
preferredFrameLatency caMetalDisplayLink =
  sendMessage caMetalDisplayLink preferredFrameLatencySelector

-- | @- setPreferredFrameLatency:@
setPreferredFrameLatency :: IsCAMetalDisplayLink caMetalDisplayLink => caMetalDisplayLink -> CFloat -> IO ()
setPreferredFrameLatency caMetalDisplayLink value =
  sendMessage caMetalDisplayLink setPreferredFrameLatencySelector value

-- | @- preferredFrameRateRange@
preferredFrameRateRange :: IsCAMetalDisplayLink caMetalDisplayLink => caMetalDisplayLink -> IO CAFrameRateRange
preferredFrameRateRange caMetalDisplayLink =
  sendMessage caMetalDisplayLink preferredFrameRateRangeSelector

-- | @- setPreferredFrameRateRange:@
setPreferredFrameRateRange :: IsCAMetalDisplayLink caMetalDisplayLink => caMetalDisplayLink -> CAFrameRateRange -> IO ()
setPreferredFrameRateRange caMetalDisplayLink value =
  sendMessage caMetalDisplayLink setPreferredFrameRateRangeSelector value

-- | @- paused@
paused :: IsCAMetalDisplayLink caMetalDisplayLink => caMetalDisplayLink -> IO Bool
paused caMetalDisplayLink =
  sendMessage caMetalDisplayLink pausedSelector

-- | @- setPaused:@
setPaused :: IsCAMetalDisplayLink caMetalDisplayLink => caMetalDisplayLink -> Bool -> IO ()
setPaused caMetalDisplayLink value =
  sendMessage caMetalDisplayLink setPausedSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMetalLayer:@
initWithMetalLayerSelector :: Selector '[Id CAMetalLayer] (Id CAMetalDisplayLink)
initWithMetalLayerSelector = mkSelector "initWithMetalLayer:"

-- | @Selector@ for @addToRunLoop:forMode:@
addToRunLoop_forModeSelector :: Selector '[Id NSRunLoop, Id NSString] ()
addToRunLoop_forModeSelector = mkSelector "addToRunLoop:forMode:"

-- | @Selector@ for @removeFromRunLoop:forMode:@
removeFromRunLoop_forModeSelector :: Selector '[Id NSRunLoop, Id NSString] ()
removeFromRunLoop_forModeSelector = mkSelector "removeFromRunLoop:forMode:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector '[] ()
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @preferredFrameLatency@
preferredFrameLatencySelector :: Selector '[] CFloat
preferredFrameLatencySelector = mkSelector "preferredFrameLatency"

-- | @Selector@ for @setPreferredFrameLatency:@
setPreferredFrameLatencySelector :: Selector '[CFloat] ()
setPreferredFrameLatencySelector = mkSelector "setPreferredFrameLatency:"

-- | @Selector@ for @preferredFrameRateRange@
preferredFrameRateRangeSelector :: Selector '[] CAFrameRateRange
preferredFrameRateRangeSelector = mkSelector "preferredFrameRateRange"

-- | @Selector@ for @setPreferredFrameRateRange:@
setPreferredFrameRateRangeSelector :: Selector '[CAFrameRateRange] ()
setPreferredFrameRateRangeSelector = mkSelector "setPreferredFrameRateRange:"

-- | @Selector@ for @paused@
pausedSelector :: Selector '[] Bool
pausedSelector = mkSelector "paused"

-- | @Selector@ for @setPaused:@
setPausedSelector :: Selector '[Bool] ()
setPausedSelector = mkSelector "setPaused:"

