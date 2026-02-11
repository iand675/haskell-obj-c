{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKKeyframeSequence@.
module ObjC.SpriteKit.SKKeyframeSequence
  ( SKKeyframeSequence
  , IsSKKeyframeSequence(..)
  , initWithKeyframeValues_times
  , initWithCapacity
  , initWithCoder
  , count
  , addKeyframeValue_time
  , removeLastKeyframe
  , removeKeyframeAtIndex
  , setKeyframeValue_forIndex
  , setKeyframeTime_forIndex
  , setKeyframeValue_time_forIndex
  , getKeyframeValueForIndex
  , getKeyframeTimeForIndex
  , sampleAtTime
  , interpolationMode
  , setInterpolationMode
  , repeatMode
  , setRepeatMode
  , initWithKeyframeValues_timesSelector
  , initWithCapacitySelector
  , initWithCoderSelector
  , countSelector
  , addKeyframeValue_timeSelector
  , removeLastKeyframeSelector
  , removeKeyframeAtIndexSelector
  , setKeyframeValue_forIndexSelector
  , setKeyframeTime_forIndexSelector
  , setKeyframeValue_time_forIndexSelector
  , getKeyframeValueForIndexSelector
  , getKeyframeTimeForIndexSelector
  , sampleAtTimeSelector
  , interpolationModeSelector
  , setInterpolationModeSelector
  , repeatModeSelector
  , setRepeatModeSelector

  -- * Enum types
  , SKInterpolationMode(SKInterpolationMode)
  , pattern SKInterpolationModeLinear
  , pattern SKInterpolationModeSpline
  , pattern SKInterpolationModeStep
  , SKRepeatMode(SKRepeatMode)
  , pattern SKRepeatModeClamp
  , pattern SKRepeatModeLoop

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.SpriteKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithKeyframeValues:times:@
initWithKeyframeValues_times :: (IsSKKeyframeSequence skKeyframeSequence, IsNSArray values, IsNSArray times) => skKeyframeSequence -> values -> times -> IO (Id SKKeyframeSequence)
initWithKeyframeValues_times skKeyframeSequence  values times =
withObjCPtr values $ \raw_values ->
  withObjCPtr times $ \raw_times ->
      sendMsg skKeyframeSequence (mkSelector "initWithKeyframeValues:times:") (retPtr retVoid) [argPtr (castPtr raw_values :: Ptr ()), argPtr (castPtr raw_times :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCapacity:@
initWithCapacity :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> CULong -> IO (Id SKKeyframeSequence)
initWithCapacity skKeyframeSequence  numItems =
  sendMsg skKeyframeSequence (mkSelector "initWithCapacity:") (retPtr retVoid) [argCULong (fromIntegral numItems)] >>= ownedObject . castPtr

-- | Support coding and decoding via NSKeyedArchiver.
--
-- ObjC selector: @- initWithCoder:@
initWithCoder :: (IsSKKeyframeSequence skKeyframeSequence, IsNSCoder aDecoder) => skKeyframeSequence -> aDecoder -> IO (Id SKKeyframeSequence)
initWithCoder skKeyframeSequence  aDecoder =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg skKeyframeSequence (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- count@
count :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> IO CULong
count skKeyframeSequence  =
  sendMsg skKeyframeSequence (mkSelector "count") retCULong []

-- | @- addKeyframeValue:time:@
addKeyframeValue_time :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> RawId -> CDouble -> IO ()
addKeyframeValue_time skKeyframeSequence  value time =
  sendMsg skKeyframeSequence (mkSelector "addKeyframeValue:time:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argCDouble (fromIntegral time)]

-- | @- removeLastKeyframe@
removeLastKeyframe :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> IO ()
removeLastKeyframe skKeyframeSequence  =
  sendMsg skKeyframeSequence (mkSelector "removeLastKeyframe") retVoid []

-- | @- removeKeyframeAtIndex:@
removeKeyframeAtIndex :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> CULong -> IO ()
removeKeyframeAtIndex skKeyframeSequence  index =
  sendMsg skKeyframeSequence (mkSelector "removeKeyframeAtIndex:") retVoid [argCULong (fromIntegral index)]

-- | @- setKeyframeValue:forIndex:@
setKeyframeValue_forIndex :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> RawId -> CULong -> IO ()
setKeyframeValue_forIndex skKeyframeSequence  value index =
  sendMsg skKeyframeSequence (mkSelector "setKeyframeValue:forIndex:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argCULong (fromIntegral index)]

-- | @- setKeyframeTime:forIndex:@
setKeyframeTime_forIndex :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> CDouble -> CULong -> IO ()
setKeyframeTime_forIndex skKeyframeSequence  time index =
  sendMsg skKeyframeSequence (mkSelector "setKeyframeTime:forIndex:") retVoid [argCDouble (fromIntegral time), argCULong (fromIntegral index)]

-- | @- setKeyframeValue:time:forIndex:@
setKeyframeValue_time_forIndex :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> RawId -> CDouble -> CULong -> IO ()
setKeyframeValue_time_forIndex skKeyframeSequence  value time index =
  sendMsg skKeyframeSequence (mkSelector "setKeyframeValue:time:forIndex:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argCDouble (fromIntegral time), argCULong (fromIntegral index)]

-- | @- getKeyframeValueForIndex:@
getKeyframeValueForIndex :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> CULong -> IO RawId
getKeyframeValueForIndex skKeyframeSequence  index =
  fmap (RawId . castPtr) $ sendMsg skKeyframeSequence (mkSelector "getKeyframeValueForIndex:") (retPtr retVoid) [argCULong (fromIntegral index)]

-- | @- getKeyframeTimeForIndex:@
getKeyframeTimeForIndex :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> CULong -> IO CDouble
getKeyframeTimeForIndex skKeyframeSequence  index =
  sendMsg skKeyframeSequence (mkSelector "getKeyframeTimeForIndex:") retCDouble [argCULong (fromIntegral index)]

-- | @- sampleAtTime:@
sampleAtTime :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> CDouble -> IO RawId
sampleAtTime skKeyframeSequence  time =
  fmap (RawId . castPtr) $ sendMsg skKeyframeSequence (mkSelector "sampleAtTime:") (retPtr retVoid) [argCDouble (fromIntegral time)]

-- | @- interpolationMode@
interpolationMode :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> IO SKInterpolationMode
interpolationMode skKeyframeSequence  =
  fmap (coerce :: CLong -> SKInterpolationMode) $ sendMsg skKeyframeSequence (mkSelector "interpolationMode") retCLong []

-- | @- setInterpolationMode:@
setInterpolationMode :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> SKInterpolationMode -> IO ()
setInterpolationMode skKeyframeSequence  value =
  sendMsg skKeyframeSequence (mkSelector "setInterpolationMode:") retVoid [argCLong (coerce value)]

-- | @- repeatMode@
repeatMode :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> IO SKRepeatMode
repeatMode skKeyframeSequence  =
  fmap (coerce :: CLong -> SKRepeatMode) $ sendMsg skKeyframeSequence (mkSelector "repeatMode") retCLong []

-- | @- setRepeatMode:@
setRepeatMode :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> SKRepeatMode -> IO ()
setRepeatMode skKeyframeSequence  value =
  sendMsg skKeyframeSequence (mkSelector "setRepeatMode:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithKeyframeValues:times:@
initWithKeyframeValues_timesSelector :: Selector
initWithKeyframeValues_timesSelector = mkSelector "initWithKeyframeValues:times:"

-- | @Selector@ for @initWithCapacity:@
initWithCapacitySelector :: Selector
initWithCapacitySelector = mkSelector "initWithCapacity:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

-- | @Selector@ for @addKeyframeValue:time:@
addKeyframeValue_timeSelector :: Selector
addKeyframeValue_timeSelector = mkSelector "addKeyframeValue:time:"

-- | @Selector@ for @removeLastKeyframe@
removeLastKeyframeSelector :: Selector
removeLastKeyframeSelector = mkSelector "removeLastKeyframe"

-- | @Selector@ for @removeKeyframeAtIndex:@
removeKeyframeAtIndexSelector :: Selector
removeKeyframeAtIndexSelector = mkSelector "removeKeyframeAtIndex:"

-- | @Selector@ for @setKeyframeValue:forIndex:@
setKeyframeValue_forIndexSelector :: Selector
setKeyframeValue_forIndexSelector = mkSelector "setKeyframeValue:forIndex:"

-- | @Selector@ for @setKeyframeTime:forIndex:@
setKeyframeTime_forIndexSelector :: Selector
setKeyframeTime_forIndexSelector = mkSelector "setKeyframeTime:forIndex:"

-- | @Selector@ for @setKeyframeValue:time:forIndex:@
setKeyframeValue_time_forIndexSelector :: Selector
setKeyframeValue_time_forIndexSelector = mkSelector "setKeyframeValue:time:forIndex:"

-- | @Selector@ for @getKeyframeValueForIndex:@
getKeyframeValueForIndexSelector :: Selector
getKeyframeValueForIndexSelector = mkSelector "getKeyframeValueForIndex:"

-- | @Selector@ for @getKeyframeTimeForIndex:@
getKeyframeTimeForIndexSelector :: Selector
getKeyframeTimeForIndexSelector = mkSelector "getKeyframeTimeForIndex:"

-- | @Selector@ for @sampleAtTime:@
sampleAtTimeSelector :: Selector
sampleAtTimeSelector = mkSelector "sampleAtTime:"

-- | @Selector@ for @interpolationMode@
interpolationModeSelector :: Selector
interpolationModeSelector = mkSelector "interpolationMode"

-- | @Selector@ for @setInterpolationMode:@
setInterpolationModeSelector :: Selector
setInterpolationModeSelector = mkSelector "setInterpolationMode:"

-- | @Selector@ for @repeatMode@
repeatModeSelector :: Selector
repeatModeSelector = mkSelector "repeatMode"

-- | @Selector@ for @setRepeatMode:@
setRepeatModeSelector :: Selector
setRepeatModeSelector = mkSelector "setRepeatMode:"

