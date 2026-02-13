{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Descriptor for a primitive acceleration structure
--
-- Generated bindings for @MTLPrimitiveAccelerationStructureDescriptor@.
module ObjC.Metal.MTLPrimitiveAccelerationStructureDescriptor
  ( MTLPrimitiveAccelerationStructureDescriptor
  , IsMTLPrimitiveAccelerationStructureDescriptor(..)
  , descriptor
  , geometryDescriptors
  , setGeometryDescriptors
  , motionStartBorderMode
  , setMotionStartBorderMode
  , motionEndBorderMode
  , setMotionEndBorderMode
  , motionStartTime
  , setMotionStartTime
  , motionEndTime
  , setMotionEndTime
  , motionKeyframeCount
  , setMotionKeyframeCount
  , descriptorSelector
  , geometryDescriptorsSelector
  , motionEndBorderModeSelector
  , motionEndTimeSelector
  , motionKeyframeCountSelector
  , motionStartBorderModeSelector
  , motionStartTimeSelector
  , setGeometryDescriptorsSelector
  , setMotionEndBorderModeSelector
  , setMotionEndTimeSelector
  , setMotionKeyframeCountSelector
  , setMotionStartBorderModeSelector
  , setMotionStartTimeSelector

  -- * Enum types
  , MTLMotionBorderMode(MTLMotionBorderMode)
  , pattern MTLMotionBorderModeClamp
  , pattern MTLMotionBorderModeVanish

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ descriptor@
descriptor :: IO (Id MTLPrimitiveAccelerationStructureDescriptor)
descriptor  =
  do
    cls' <- getRequiredClass "MTLPrimitiveAccelerationStructureDescriptor"
    sendClassMessage cls' descriptorSelector

-- | Array of geometry descriptors. If motionKeyframeCount is greater than one all geometryDescriptors must be motion versions and have motionKeyframeCount of primitive buffers.
--
-- ObjC selector: @- geometryDescriptors@
geometryDescriptors :: IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor => mtlPrimitiveAccelerationStructureDescriptor -> IO (Id NSArray)
geometryDescriptors mtlPrimitiveAccelerationStructureDescriptor =
  sendMessage mtlPrimitiveAccelerationStructureDescriptor geometryDescriptorsSelector

-- | Array of geometry descriptors. If motionKeyframeCount is greater than one all geometryDescriptors must be motion versions and have motionKeyframeCount of primitive buffers.
--
-- ObjC selector: @- setGeometryDescriptors:@
setGeometryDescriptors :: (IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor, IsNSArray value) => mtlPrimitiveAccelerationStructureDescriptor -> value -> IO ()
setGeometryDescriptors mtlPrimitiveAccelerationStructureDescriptor value =
  sendMessage mtlPrimitiveAccelerationStructureDescriptor setGeometryDescriptorsSelector (toNSArray value)

-- | Motion border mode describing what happens if acceleration structure is sampled before motionStartTime. If not set defaults to MTLMotionBorderModeClamp.
--
-- ObjC selector: @- motionStartBorderMode@
motionStartBorderMode :: IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor => mtlPrimitiveAccelerationStructureDescriptor -> IO MTLMotionBorderMode
motionStartBorderMode mtlPrimitiveAccelerationStructureDescriptor =
  sendMessage mtlPrimitiveAccelerationStructureDescriptor motionStartBorderModeSelector

-- | Motion border mode describing what happens if acceleration structure is sampled before motionStartTime. If not set defaults to MTLMotionBorderModeClamp.
--
-- ObjC selector: @- setMotionStartBorderMode:@
setMotionStartBorderMode :: IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor => mtlPrimitiveAccelerationStructureDescriptor -> MTLMotionBorderMode -> IO ()
setMotionStartBorderMode mtlPrimitiveAccelerationStructureDescriptor value =
  sendMessage mtlPrimitiveAccelerationStructureDescriptor setMotionStartBorderModeSelector value

-- | Motion border mode describing what happens if acceleration structure is sampled after motionEndTime. If not set defaults to MTLMotionBorderModeClamp.
--
-- ObjC selector: @- motionEndBorderMode@
motionEndBorderMode :: IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor => mtlPrimitiveAccelerationStructureDescriptor -> IO MTLMotionBorderMode
motionEndBorderMode mtlPrimitiveAccelerationStructureDescriptor =
  sendMessage mtlPrimitiveAccelerationStructureDescriptor motionEndBorderModeSelector

-- | Motion border mode describing what happens if acceleration structure is sampled after motionEndTime. If not set defaults to MTLMotionBorderModeClamp.
--
-- ObjC selector: @- setMotionEndBorderMode:@
setMotionEndBorderMode :: IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor => mtlPrimitiveAccelerationStructureDescriptor -> MTLMotionBorderMode -> IO ()
setMotionEndBorderMode mtlPrimitiveAccelerationStructureDescriptor value =
  sendMessage mtlPrimitiveAccelerationStructureDescriptor setMotionEndBorderModeSelector value

-- | Motion start time of this geometry. If not set defaults to 0.0f.
--
-- ObjC selector: @- motionStartTime@
motionStartTime :: IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor => mtlPrimitiveAccelerationStructureDescriptor -> IO CFloat
motionStartTime mtlPrimitiveAccelerationStructureDescriptor =
  sendMessage mtlPrimitiveAccelerationStructureDescriptor motionStartTimeSelector

-- | Motion start time of this geometry. If not set defaults to 0.0f.
--
-- ObjC selector: @- setMotionStartTime:@
setMotionStartTime :: IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor => mtlPrimitiveAccelerationStructureDescriptor -> CFloat -> IO ()
setMotionStartTime mtlPrimitiveAccelerationStructureDescriptor value =
  sendMessage mtlPrimitiveAccelerationStructureDescriptor setMotionStartTimeSelector value

-- | Motion end time of this geometry. If not set defaults to 1.0f.
--
-- ObjC selector: @- motionEndTime@
motionEndTime :: IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor => mtlPrimitiveAccelerationStructureDescriptor -> IO CFloat
motionEndTime mtlPrimitiveAccelerationStructureDescriptor =
  sendMessage mtlPrimitiveAccelerationStructureDescriptor motionEndTimeSelector

-- | Motion end time of this geometry. If not set defaults to 1.0f.
--
-- ObjC selector: @- setMotionEndTime:@
setMotionEndTime :: IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor => mtlPrimitiveAccelerationStructureDescriptor -> CFloat -> IO ()
setMotionEndTime mtlPrimitiveAccelerationStructureDescriptor value =
  sendMessage mtlPrimitiveAccelerationStructureDescriptor setMotionEndTimeSelector value

-- | Motion keyframe count. Is 1 by default which means no motion.
--
-- ObjC selector: @- motionKeyframeCount@
motionKeyframeCount :: IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor => mtlPrimitiveAccelerationStructureDescriptor -> IO CULong
motionKeyframeCount mtlPrimitiveAccelerationStructureDescriptor =
  sendMessage mtlPrimitiveAccelerationStructureDescriptor motionKeyframeCountSelector

-- | Motion keyframe count. Is 1 by default which means no motion.
--
-- ObjC selector: @- setMotionKeyframeCount:@
setMotionKeyframeCount :: IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor => mtlPrimitiveAccelerationStructureDescriptor -> CULong -> IO ()
setMotionKeyframeCount mtlPrimitiveAccelerationStructureDescriptor value =
  sendMessage mtlPrimitiveAccelerationStructureDescriptor setMotionKeyframeCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id MTLPrimitiveAccelerationStructureDescriptor)
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @geometryDescriptors@
geometryDescriptorsSelector :: Selector '[] (Id NSArray)
geometryDescriptorsSelector = mkSelector "geometryDescriptors"

-- | @Selector@ for @setGeometryDescriptors:@
setGeometryDescriptorsSelector :: Selector '[Id NSArray] ()
setGeometryDescriptorsSelector = mkSelector "setGeometryDescriptors:"

-- | @Selector@ for @motionStartBorderMode@
motionStartBorderModeSelector :: Selector '[] MTLMotionBorderMode
motionStartBorderModeSelector = mkSelector "motionStartBorderMode"

-- | @Selector@ for @setMotionStartBorderMode:@
setMotionStartBorderModeSelector :: Selector '[MTLMotionBorderMode] ()
setMotionStartBorderModeSelector = mkSelector "setMotionStartBorderMode:"

-- | @Selector@ for @motionEndBorderMode@
motionEndBorderModeSelector :: Selector '[] MTLMotionBorderMode
motionEndBorderModeSelector = mkSelector "motionEndBorderMode"

-- | @Selector@ for @setMotionEndBorderMode:@
setMotionEndBorderModeSelector :: Selector '[MTLMotionBorderMode] ()
setMotionEndBorderModeSelector = mkSelector "setMotionEndBorderMode:"

-- | @Selector@ for @motionStartTime@
motionStartTimeSelector :: Selector '[] CFloat
motionStartTimeSelector = mkSelector "motionStartTime"

-- | @Selector@ for @setMotionStartTime:@
setMotionStartTimeSelector :: Selector '[CFloat] ()
setMotionStartTimeSelector = mkSelector "setMotionStartTime:"

-- | @Selector@ for @motionEndTime@
motionEndTimeSelector :: Selector '[] CFloat
motionEndTimeSelector = mkSelector "motionEndTime"

-- | @Selector@ for @setMotionEndTime:@
setMotionEndTimeSelector :: Selector '[CFloat] ()
setMotionEndTimeSelector = mkSelector "setMotionEndTime:"

-- | @Selector@ for @motionKeyframeCount@
motionKeyframeCountSelector :: Selector '[] CULong
motionKeyframeCountSelector = mkSelector "motionKeyframeCount"

-- | @Selector@ for @setMotionKeyframeCount:@
setMotionKeyframeCountSelector :: Selector '[CULong] ()
setMotionKeyframeCountSelector = mkSelector "setMotionKeyframeCount:"

