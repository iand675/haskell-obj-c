{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct@.
module ObjC.Matter.MTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct
  ( MTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct
  , IsMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct(..)
  , initialDuration
  , setInitialDuration
  , augmentationDuration
  , setAugmentationDuration
  , maxDuration
  , setMaxDuration
  , blindDuration
  , setBlindDuration
  , augmentationDurationSelector
  , blindDurationSelector
  , initialDurationSelector
  , maxDurationSelector
  , setAugmentationDurationSelector
  , setBlindDurationSelector
  , setInitialDurationSelector
  , setMaxDurationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initialDuration@
initialDuration :: IsMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct => mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct -> IO (Id NSNumber)
initialDuration mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct =
  sendOwnedMessage mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct initialDurationSelector

-- | @- setInitialDuration:@
setInitialDuration :: (IsMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct -> value -> IO ()
setInitialDuration mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct setInitialDurationSelector (toNSNumber value)

-- | @- augmentationDuration@
augmentationDuration :: IsMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct => mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct -> IO (Id NSNumber)
augmentationDuration mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct =
  sendMessage mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct augmentationDurationSelector

-- | @- setAugmentationDuration:@
setAugmentationDuration :: (IsMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct -> value -> IO ()
setAugmentationDuration mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct setAugmentationDurationSelector (toNSNumber value)

-- | @- maxDuration@
maxDuration :: IsMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct => mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct -> IO (Id NSNumber)
maxDuration mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct =
  sendMessage mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct maxDurationSelector

-- | @- setMaxDuration:@
setMaxDuration :: (IsMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct -> value -> IO ()
setMaxDuration mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct setMaxDurationSelector (toNSNumber value)

-- | @- blindDuration@
blindDuration :: IsMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct => mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct -> IO (Id NSNumber)
blindDuration mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct =
  sendMessage mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct blindDurationSelector

-- | @- setBlindDuration:@
setBlindDuration :: (IsMTRPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct -> value -> IO ()
setBlindDuration mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportMotionTriggerTimeControlStruct setBlindDurationSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initialDuration@
initialDurationSelector :: Selector '[] (Id NSNumber)
initialDurationSelector = mkSelector "initialDuration"

-- | @Selector@ for @setInitialDuration:@
setInitialDurationSelector :: Selector '[Id NSNumber] ()
setInitialDurationSelector = mkSelector "setInitialDuration:"

-- | @Selector@ for @augmentationDuration@
augmentationDurationSelector :: Selector '[] (Id NSNumber)
augmentationDurationSelector = mkSelector "augmentationDuration"

-- | @Selector@ for @setAugmentationDuration:@
setAugmentationDurationSelector :: Selector '[Id NSNumber] ()
setAugmentationDurationSelector = mkSelector "setAugmentationDuration:"

-- | @Selector@ for @maxDuration@
maxDurationSelector :: Selector '[] (Id NSNumber)
maxDurationSelector = mkSelector "maxDuration"

-- | @Selector@ for @setMaxDuration:@
setMaxDurationSelector :: Selector '[Id NSNumber] ()
setMaxDurationSelector = mkSelector "setMaxDuration:"

-- | @Selector@ for @blindDuration@
blindDurationSelector :: Selector '[] (Id NSNumber)
blindDurationSelector = mkSelector "blindDuration"

-- | @Selector@ for @setBlindDuration:@
setBlindDurationSelector :: Selector '[Id NSNumber] ()
setBlindDurationSelector = mkSelector "setBlindDuration:"

