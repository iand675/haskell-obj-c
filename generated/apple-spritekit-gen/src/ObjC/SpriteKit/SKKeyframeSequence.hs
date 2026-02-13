{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , addKeyframeValue_timeSelector
  , countSelector
  , getKeyframeTimeForIndexSelector
  , getKeyframeValueForIndexSelector
  , initWithCapacitySelector
  , initWithCoderSelector
  , initWithKeyframeValues_timesSelector
  , interpolationModeSelector
  , removeKeyframeAtIndexSelector
  , removeLastKeyframeSelector
  , repeatModeSelector
  , sampleAtTimeSelector
  , setInterpolationModeSelector
  , setKeyframeTime_forIndexSelector
  , setKeyframeValue_forIndexSelector
  , setKeyframeValue_time_forIndexSelector
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.SpriteKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithKeyframeValues:times:@
initWithKeyframeValues_times :: (IsSKKeyframeSequence skKeyframeSequence, IsNSArray values, IsNSArray times) => skKeyframeSequence -> values -> times -> IO (Id SKKeyframeSequence)
initWithKeyframeValues_times skKeyframeSequence values times =
  sendOwnedMessage skKeyframeSequence initWithKeyframeValues_timesSelector (toNSArray values) (toNSArray times)

-- | @- initWithCapacity:@
initWithCapacity :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> CULong -> IO (Id SKKeyframeSequence)
initWithCapacity skKeyframeSequence numItems =
  sendOwnedMessage skKeyframeSequence initWithCapacitySelector numItems

-- | Support coding and decoding via NSKeyedArchiver.
--
-- ObjC selector: @- initWithCoder:@
initWithCoder :: (IsSKKeyframeSequence skKeyframeSequence, IsNSCoder aDecoder) => skKeyframeSequence -> aDecoder -> IO (Id SKKeyframeSequence)
initWithCoder skKeyframeSequence aDecoder =
  sendOwnedMessage skKeyframeSequence initWithCoderSelector (toNSCoder aDecoder)

-- | @- count@
count :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> IO CULong
count skKeyframeSequence =
  sendMessage skKeyframeSequence countSelector

-- | @- addKeyframeValue:time:@
addKeyframeValue_time :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> RawId -> CDouble -> IO ()
addKeyframeValue_time skKeyframeSequence value time =
  sendMessage skKeyframeSequence addKeyframeValue_timeSelector value time

-- | @- removeLastKeyframe@
removeLastKeyframe :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> IO ()
removeLastKeyframe skKeyframeSequence =
  sendMessage skKeyframeSequence removeLastKeyframeSelector

-- | @- removeKeyframeAtIndex:@
removeKeyframeAtIndex :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> CULong -> IO ()
removeKeyframeAtIndex skKeyframeSequence index =
  sendMessage skKeyframeSequence removeKeyframeAtIndexSelector index

-- | @- setKeyframeValue:forIndex:@
setKeyframeValue_forIndex :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> RawId -> CULong -> IO ()
setKeyframeValue_forIndex skKeyframeSequence value index =
  sendMessage skKeyframeSequence setKeyframeValue_forIndexSelector value index

-- | @- setKeyframeTime:forIndex:@
setKeyframeTime_forIndex :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> CDouble -> CULong -> IO ()
setKeyframeTime_forIndex skKeyframeSequence time index =
  sendMessage skKeyframeSequence setKeyframeTime_forIndexSelector time index

-- | @- setKeyframeValue:time:forIndex:@
setKeyframeValue_time_forIndex :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> RawId -> CDouble -> CULong -> IO ()
setKeyframeValue_time_forIndex skKeyframeSequence value time index =
  sendMessage skKeyframeSequence setKeyframeValue_time_forIndexSelector value time index

-- | @- getKeyframeValueForIndex:@
getKeyframeValueForIndex :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> CULong -> IO RawId
getKeyframeValueForIndex skKeyframeSequence index =
  sendMessage skKeyframeSequence getKeyframeValueForIndexSelector index

-- | @- getKeyframeTimeForIndex:@
getKeyframeTimeForIndex :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> CULong -> IO CDouble
getKeyframeTimeForIndex skKeyframeSequence index =
  sendMessage skKeyframeSequence getKeyframeTimeForIndexSelector index

-- | @- sampleAtTime:@
sampleAtTime :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> CDouble -> IO RawId
sampleAtTime skKeyframeSequence time =
  sendMessage skKeyframeSequence sampleAtTimeSelector time

-- | @- interpolationMode@
interpolationMode :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> IO SKInterpolationMode
interpolationMode skKeyframeSequence =
  sendMessage skKeyframeSequence interpolationModeSelector

-- | @- setInterpolationMode:@
setInterpolationMode :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> SKInterpolationMode -> IO ()
setInterpolationMode skKeyframeSequence value =
  sendMessage skKeyframeSequence setInterpolationModeSelector value

-- | @- repeatMode@
repeatMode :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> IO SKRepeatMode
repeatMode skKeyframeSequence =
  sendMessage skKeyframeSequence repeatModeSelector

-- | @- setRepeatMode:@
setRepeatMode :: IsSKKeyframeSequence skKeyframeSequence => skKeyframeSequence -> SKRepeatMode -> IO ()
setRepeatMode skKeyframeSequence value =
  sendMessage skKeyframeSequence setRepeatModeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithKeyframeValues:times:@
initWithKeyframeValues_timesSelector :: Selector '[Id NSArray, Id NSArray] (Id SKKeyframeSequence)
initWithKeyframeValues_timesSelector = mkSelector "initWithKeyframeValues:times:"

-- | @Selector@ for @initWithCapacity:@
initWithCapacitySelector :: Selector '[CULong] (Id SKKeyframeSequence)
initWithCapacitySelector = mkSelector "initWithCapacity:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id SKKeyframeSequence)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @count@
countSelector :: Selector '[] CULong
countSelector = mkSelector "count"

-- | @Selector@ for @addKeyframeValue:time:@
addKeyframeValue_timeSelector :: Selector '[RawId, CDouble] ()
addKeyframeValue_timeSelector = mkSelector "addKeyframeValue:time:"

-- | @Selector@ for @removeLastKeyframe@
removeLastKeyframeSelector :: Selector '[] ()
removeLastKeyframeSelector = mkSelector "removeLastKeyframe"

-- | @Selector@ for @removeKeyframeAtIndex:@
removeKeyframeAtIndexSelector :: Selector '[CULong] ()
removeKeyframeAtIndexSelector = mkSelector "removeKeyframeAtIndex:"

-- | @Selector@ for @setKeyframeValue:forIndex:@
setKeyframeValue_forIndexSelector :: Selector '[RawId, CULong] ()
setKeyframeValue_forIndexSelector = mkSelector "setKeyframeValue:forIndex:"

-- | @Selector@ for @setKeyframeTime:forIndex:@
setKeyframeTime_forIndexSelector :: Selector '[CDouble, CULong] ()
setKeyframeTime_forIndexSelector = mkSelector "setKeyframeTime:forIndex:"

-- | @Selector@ for @setKeyframeValue:time:forIndex:@
setKeyframeValue_time_forIndexSelector :: Selector '[RawId, CDouble, CULong] ()
setKeyframeValue_time_forIndexSelector = mkSelector "setKeyframeValue:time:forIndex:"

-- | @Selector@ for @getKeyframeValueForIndex:@
getKeyframeValueForIndexSelector :: Selector '[CULong] RawId
getKeyframeValueForIndexSelector = mkSelector "getKeyframeValueForIndex:"

-- | @Selector@ for @getKeyframeTimeForIndex:@
getKeyframeTimeForIndexSelector :: Selector '[CULong] CDouble
getKeyframeTimeForIndexSelector = mkSelector "getKeyframeTimeForIndex:"

-- | @Selector@ for @sampleAtTime:@
sampleAtTimeSelector :: Selector '[CDouble] RawId
sampleAtTimeSelector = mkSelector "sampleAtTime:"

-- | @Selector@ for @interpolationMode@
interpolationModeSelector :: Selector '[] SKInterpolationMode
interpolationModeSelector = mkSelector "interpolationMode"

-- | @Selector@ for @setInterpolationMode:@
setInterpolationModeSelector :: Selector '[SKInterpolationMode] ()
setInterpolationModeSelector = mkSelector "setInterpolationMode:"

-- | @Selector@ for @repeatMode@
repeatModeSelector :: Selector '[] SKRepeatMode
repeatModeSelector = mkSelector "repeatMode"

-- | @Selector@ for @setRepeatMode:@
setRepeatModeSelector :: Selector '[SKRepeatMode] ()
setRepeatModeSelector = mkSelector "setRepeatMode:"

