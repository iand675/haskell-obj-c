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
  , preferredFrameLatency
  , setPreferredFrameLatency
  , preferredFrameRateRange
  , setPreferredFrameRateRange
  , paused
  , setPaused
  , initWithMetalLayerSelector
  , addToRunLoop_forModeSelector
  , removeFromRunLoop_forModeSelector
  , invalidateSelector
  , preferredFrameLatencySelector
  , setPreferredFrameLatencySelector
  , preferredFrameRateRangeSelector
  , setPreferredFrameRateRangeSelector
  , pausedSelector
  , setPausedSelector


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

-- | @- initWithMetalLayer:@
initWithMetalLayer :: (IsCAMetalDisplayLink caMetalDisplayLink, IsCAMetalLayer layer) => caMetalDisplayLink -> layer -> IO (Id CAMetalDisplayLink)
initWithMetalLayer caMetalDisplayLink  layer =
withObjCPtr layer $ \raw_layer ->
    sendMsg caMetalDisplayLink (mkSelector "initWithMetalLayer:") (retPtr retVoid) [argPtr (castPtr raw_layer :: Ptr ())] >>= ownedObject . castPtr

-- | @- addToRunLoop:forMode:@
addToRunLoop_forMode :: (IsCAMetalDisplayLink caMetalDisplayLink, IsNSRunLoop runloop, IsNSString mode) => caMetalDisplayLink -> runloop -> mode -> IO ()
addToRunLoop_forMode caMetalDisplayLink  runloop mode =
withObjCPtr runloop $ \raw_runloop ->
  withObjCPtr mode $ \raw_mode ->
      sendMsg caMetalDisplayLink (mkSelector "addToRunLoop:forMode:") retVoid [argPtr (castPtr raw_runloop :: Ptr ()), argPtr (castPtr raw_mode :: Ptr ())]

-- | @- removeFromRunLoop:forMode:@
removeFromRunLoop_forMode :: (IsCAMetalDisplayLink caMetalDisplayLink, IsNSRunLoop runloop, IsNSString mode) => caMetalDisplayLink -> runloop -> mode -> IO ()
removeFromRunLoop_forMode caMetalDisplayLink  runloop mode =
withObjCPtr runloop $ \raw_runloop ->
  withObjCPtr mode $ \raw_mode ->
      sendMsg caMetalDisplayLink (mkSelector "removeFromRunLoop:forMode:") retVoid [argPtr (castPtr raw_runloop :: Ptr ()), argPtr (castPtr raw_mode :: Ptr ())]

-- | @- invalidate@
invalidate :: IsCAMetalDisplayLink caMetalDisplayLink => caMetalDisplayLink -> IO ()
invalidate caMetalDisplayLink  =
  sendMsg caMetalDisplayLink (mkSelector "invalidate") retVoid []

-- | @- preferredFrameLatency@
preferredFrameLatency :: IsCAMetalDisplayLink caMetalDisplayLink => caMetalDisplayLink -> IO CFloat
preferredFrameLatency caMetalDisplayLink  =
  sendMsg caMetalDisplayLink (mkSelector "preferredFrameLatency") retCFloat []

-- | @- setPreferredFrameLatency:@
setPreferredFrameLatency :: IsCAMetalDisplayLink caMetalDisplayLink => caMetalDisplayLink -> CFloat -> IO ()
setPreferredFrameLatency caMetalDisplayLink  value =
  sendMsg caMetalDisplayLink (mkSelector "setPreferredFrameLatency:") retVoid [argCFloat (fromIntegral value)]

-- | @- preferredFrameRateRange@
preferredFrameRateRange :: IsCAMetalDisplayLink caMetalDisplayLink => caMetalDisplayLink -> IO CAFrameRateRange
preferredFrameRateRange caMetalDisplayLink  =
  sendMsgStret caMetalDisplayLink (mkSelector "preferredFrameRateRange") retCAFrameRateRange []

-- | @- setPreferredFrameRateRange:@
setPreferredFrameRateRange :: IsCAMetalDisplayLink caMetalDisplayLink => caMetalDisplayLink -> CAFrameRateRange -> IO ()
setPreferredFrameRateRange caMetalDisplayLink  value =
  sendMsg caMetalDisplayLink (mkSelector "setPreferredFrameRateRange:") retVoid [argCAFrameRateRange value]

-- | @- paused@
paused :: IsCAMetalDisplayLink caMetalDisplayLink => caMetalDisplayLink -> IO Bool
paused caMetalDisplayLink  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg caMetalDisplayLink (mkSelector "paused") retCULong []

-- | @- setPaused:@
setPaused :: IsCAMetalDisplayLink caMetalDisplayLink => caMetalDisplayLink -> Bool -> IO ()
setPaused caMetalDisplayLink  value =
  sendMsg caMetalDisplayLink (mkSelector "setPaused:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMetalLayer:@
initWithMetalLayerSelector :: Selector
initWithMetalLayerSelector = mkSelector "initWithMetalLayer:"

-- | @Selector@ for @addToRunLoop:forMode:@
addToRunLoop_forModeSelector :: Selector
addToRunLoop_forModeSelector = mkSelector "addToRunLoop:forMode:"

-- | @Selector@ for @removeFromRunLoop:forMode:@
removeFromRunLoop_forModeSelector :: Selector
removeFromRunLoop_forModeSelector = mkSelector "removeFromRunLoop:forMode:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @preferredFrameLatency@
preferredFrameLatencySelector :: Selector
preferredFrameLatencySelector = mkSelector "preferredFrameLatency"

-- | @Selector@ for @setPreferredFrameLatency:@
setPreferredFrameLatencySelector :: Selector
setPreferredFrameLatencySelector = mkSelector "setPreferredFrameLatency:"

-- | @Selector@ for @preferredFrameRateRange@
preferredFrameRateRangeSelector :: Selector
preferredFrameRateRangeSelector = mkSelector "preferredFrameRateRange"

-- | @Selector@ for @setPreferredFrameRateRange:@
setPreferredFrameRateRangeSelector :: Selector
setPreferredFrameRateRangeSelector = mkSelector "setPreferredFrameRateRange:"

-- | @Selector@ for @paused@
pausedSelector :: Selector
pausedSelector = mkSelector "paused"

-- | @Selector@ for @setPaused:@
setPausedSelector :: Selector
setPausedSelector = mkSelector "setPaused:"

