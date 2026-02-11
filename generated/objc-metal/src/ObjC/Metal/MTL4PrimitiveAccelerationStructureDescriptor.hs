{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Descriptor for a primitive acceleration structure that directly references geometric shapes, such as triangles and bounding boxes.
--
-- Generated bindings for @MTL4PrimitiveAccelerationStructureDescriptor@.
module ObjC.Metal.MTL4PrimitiveAccelerationStructureDescriptor
  ( MTL4PrimitiveAccelerationStructureDescriptor
  , IsMTL4PrimitiveAccelerationStructureDescriptor(..)
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
  , geometryDescriptorsSelector
  , setGeometryDescriptorsSelector
  , motionStartBorderModeSelector
  , setMotionStartBorderModeSelector
  , motionEndBorderModeSelector
  , setMotionEndBorderModeSelector
  , motionStartTimeSelector
  , setMotionStartTimeSelector
  , motionEndTimeSelector
  , setMotionEndTimeSelector
  , motionKeyframeCountSelector
  , setMotionKeyframeCountSelector

  -- * Enum types
  , MTLMotionBorderMode(MTLMotionBorderMode)
  , pattern MTLMotionBorderModeClamp
  , pattern MTLMotionBorderModeVanish

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

import ObjC.Metal.Internal.Classes
import ObjC.Metal.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Associates the array of geometry descriptors that comprise this primitive acceleration structure.
--
-- If you enable keyframe motion by setting property ``motionKeyframeCount`` to a value greater than @1@, then all geometry descriptors this array references need to be motion geometry descriptors and have a number of primitive buffers equals to ``motionKeyframeCount``.
--
-- Example of motion geometry descriptors include: ``MTL4AccelerationStructureMotionTriangleGeometryDescriptor``, ``MTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor``, ``MTL4AccelerationStructureMotionCurveGeometryDescriptor``.
--
-- ObjC selector: @- geometryDescriptors@
geometryDescriptors :: IsMTL4PrimitiveAccelerationStructureDescriptor mtL4PrimitiveAccelerationStructureDescriptor => mtL4PrimitiveAccelerationStructureDescriptor -> IO (Id NSArray)
geometryDescriptors mtL4PrimitiveAccelerationStructureDescriptor  =
  sendMsg mtL4PrimitiveAccelerationStructureDescriptor (mkSelector "geometryDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Associates the array of geometry descriptors that comprise this primitive acceleration structure.
--
-- If you enable keyframe motion by setting property ``motionKeyframeCount`` to a value greater than @1@, then all geometry descriptors this array references need to be motion geometry descriptors and have a number of primitive buffers equals to ``motionKeyframeCount``.
--
-- Example of motion geometry descriptors include: ``MTL4AccelerationStructureMotionTriangleGeometryDescriptor``, ``MTL4AccelerationStructureMotionBoundingBoxGeometryDescriptor``, ``MTL4AccelerationStructureMotionCurveGeometryDescriptor``.
--
-- ObjC selector: @- setGeometryDescriptors:@
setGeometryDescriptors :: (IsMTL4PrimitiveAccelerationStructureDescriptor mtL4PrimitiveAccelerationStructureDescriptor, IsNSArray value) => mtL4PrimitiveAccelerationStructureDescriptor -> value -> IO ()
setGeometryDescriptors mtL4PrimitiveAccelerationStructureDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4PrimitiveAccelerationStructureDescriptor (mkSelector "setGeometryDescriptors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Configures the behavior when the ray-tracing system samples the acceleration structure before the motion start time.
--
-- Use this property to control the behavior when the ray-tracing system samples the acceleration structure at a time prior to the one you set for ``motionStartTime``.
--
-- The default value of this property is @MTLMotionBorderModeClamp@.
--
-- ObjC selector: @- motionStartBorderMode@
motionStartBorderMode :: IsMTL4PrimitiveAccelerationStructureDescriptor mtL4PrimitiveAccelerationStructureDescriptor => mtL4PrimitiveAccelerationStructureDescriptor -> IO MTLMotionBorderMode
motionStartBorderMode mtL4PrimitiveAccelerationStructureDescriptor  =
  fmap (coerce :: CUInt -> MTLMotionBorderMode) $ sendMsg mtL4PrimitiveAccelerationStructureDescriptor (mkSelector "motionStartBorderMode") retCUInt []

-- | Configures the behavior when the ray-tracing system samples the acceleration structure before the motion start time.
--
-- Use this property to control the behavior when the ray-tracing system samples the acceleration structure at a time prior to the one you set for ``motionStartTime``.
--
-- The default value of this property is @MTLMotionBorderModeClamp@.
--
-- ObjC selector: @- setMotionStartBorderMode:@
setMotionStartBorderMode :: IsMTL4PrimitiveAccelerationStructureDescriptor mtL4PrimitiveAccelerationStructureDescriptor => mtL4PrimitiveAccelerationStructureDescriptor -> MTLMotionBorderMode -> IO ()
setMotionStartBorderMode mtL4PrimitiveAccelerationStructureDescriptor  value =
  sendMsg mtL4PrimitiveAccelerationStructureDescriptor (mkSelector "setMotionStartBorderMode:") retVoid [argCUInt (coerce value)]

-- | Configures the motion border mode.
--
-- This property controls what happens if Metal samples the acceleration structure after ``motionEndTime``.
--
-- Its default value is @MTLMotionBorderModeClamp@.
--
-- ObjC selector: @- motionEndBorderMode@
motionEndBorderMode :: IsMTL4PrimitiveAccelerationStructureDescriptor mtL4PrimitiveAccelerationStructureDescriptor => mtL4PrimitiveAccelerationStructureDescriptor -> IO MTLMotionBorderMode
motionEndBorderMode mtL4PrimitiveAccelerationStructureDescriptor  =
  fmap (coerce :: CUInt -> MTLMotionBorderMode) $ sendMsg mtL4PrimitiveAccelerationStructureDescriptor (mkSelector "motionEndBorderMode") retCUInt []

-- | Configures the motion border mode.
--
-- This property controls what happens if Metal samples the acceleration structure after ``motionEndTime``.
--
-- Its default value is @MTLMotionBorderModeClamp@.
--
-- ObjC selector: @- setMotionEndBorderMode:@
setMotionEndBorderMode :: IsMTL4PrimitiveAccelerationStructureDescriptor mtL4PrimitiveAccelerationStructureDescriptor => mtL4PrimitiveAccelerationStructureDescriptor -> MTLMotionBorderMode -> IO ()
setMotionEndBorderMode mtL4PrimitiveAccelerationStructureDescriptor  value =
  sendMsg mtL4PrimitiveAccelerationStructureDescriptor (mkSelector "setMotionEndBorderMode:") retVoid [argCUInt (coerce value)]

-- | Configures the motion start time for this geometry.
--
-- The default value of this property is @0.0f@.
--
-- ObjC selector: @- motionStartTime@
motionStartTime :: IsMTL4PrimitiveAccelerationStructureDescriptor mtL4PrimitiveAccelerationStructureDescriptor => mtL4PrimitiveAccelerationStructureDescriptor -> IO CFloat
motionStartTime mtL4PrimitiveAccelerationStructureDescriptor  =
  sendMsg mtL4PrimitiveAccelerationStructureDescriptor (mkSelector "motionStartTime") retCFloat []

-- | Configures the motion start time for this geometry.
--
-- The default value of this property is @0.0f@.
--
-- ObjC selector: @- setMotionStartTime:@
setMotionStartTime :: IsMTL4PrimitiveAccelerationStructureDescriptor mtL4PrimitiveAccelerationStructureDescriptor => mtL4PrimitiveAccelerationStructureDescriptor -> CFloat -> IO ()
setMotionStartTime mtL4PrimitiveAccelerationStructureDescriptor  value =
  sendMsg mtL4PrimitiveAccelerationStructureDescriptor (mkSelector "setMotionStartTime:") retVoid [argCFloat (fromIntegral value)]

-- | Configures the motion end time for this geometry.
--
-- The default value of this property is @1.0f@.
--
-- ObjC selector: @- motionEndTime@
motionEndTime :: IsMTL4PrimitiveAccelerationStructureDescriptor mtL4PrimitiveAccelerationStructureDescriptor => mtL4PrimitiveAccelerationStructureDescriptor -> IO CFloat
motionEndTime mtL4PrimitiveAccelerationStructureDescriptor  =
  sendMsg mtL4PrimitiveAccelerationStructureDescriptor (mkSelector "motionEndTime") retCFloat []

-- | Configures the motion end time for this geometry.
--
-- The default value of this property is @1.0f@.
--
-- ObjC selector: @- setMotionEndTime:@
setMotionEndTime :: IsMTL4PrimitiveAccelerationStructureDescriptor mtL4PrimitiveAccelerationStructureDescriptor => mtL4PrimitiveAccelerationStructureDescriptor -> CFloat -> IO ()
setMotionEndTime mtL4PrimitiveAccelerationStructureDescriptor  value =
  sendMsg mtL4PrimitiveAccelerationStructureDescriptor (mkSelector "setMotionEndTime:") retVoid [argCFloat (fromIntegral value)]

-- | Sets the motion keyframe count.
--
-- This property's default is @1@, indicating no motion.
--
-- ObjC selector: @- motionKeyframeCount@
motionKeyframeCount :: IsMTL4PrimitiveAccelerationStructureDescriptor mtL4PrimitiveAccelerationStructureDescriptor => mtL4PrimitiveAccelerationStructureDescriptor -> IO CULong
motionKeyframeCount mtL4PrimitiveAccelerationStructureDescriptor  =
  sendMsg mtL4PrimitiveAccelerationStructureDescriptor (mkSelector "motionKeyframeCount") retCULong []

-- | Sets the motion keyframe count.
--
-- This property's default is @1@, indicating no motion.
--
-- ObjC selector: @- setMotionKeyframeCount:@
setMotionKeyframeCount :: IsMTL4PrimitiveAccelerationStructureDescriptor mtL4PrimitiveAccelerationStructureDescriptor => mtL4PrimitiveAccelerationStructureDescriptor -> CULong -> IO ()
setMotionKeyframeCount mtL4PrimitiveAccelerationStructureDescriptor  value =
  sendMsg mtL4PrimitiveAccelerationStructureDescriptor (mkSelector "setMotionKeyframeCount:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @geometryDescriptors@
geometryDescriptorsSelector :: Selector
geometryDescriptorsSelector = mkSelector "geometryDescriptors"

-- | @Selector@ for @setGeometryDescriptors:@
setGeometryDescriptorsSelector :: Selector
setGeometryDescriptorsSelector = mkSelector "setGeometryDescriptors:"

-- | @Selector@ for @motionStartBorderMode@
motionStartBorderModeSelector :: Selector
motionStartBorderModeSelector = mkSelector "motionStartBorderMode"

-- | @Selector@ for @setMotionStartBorderMode:@
setMotionStartBorderModeSelector :: Selector
setMotionStartBorderModeSelector = mkSelector "setMotionStartBorderMode:"

-- | @Selector@ for @motionEndBorderMode@
motionEndBorderModeSelector :: Selector
motionEndBorderModeSelector = mkSelector "motionEndBorderMode"

-- | @Selector@ for @setMotionEndBorderMode:@
setMotionEndBorderModeSelector :: Selector
setMotionEndBorderModeSelector = mkSelector "setMotionEndBorderMode:"

-- | @Selector@ for @motionStartTime@
motionStartTimeSelector :: Selector
motionStartTimeSelector = mkSelector "motionStartTime"

-- | @Selector@ for @setMotionStartTime:@
setMotionStartTimeSelector :: Selector
setMotionStartTimeSelector = mkSelector "setMotionStartTime:"

-- | @Selector@ for @motionEndTime@
motionEndTimeSelector :: Selector
motionEndTimeSelector = mkSelector "motionEndTime"

-- | @Selector@ for @setMotionEndTime:@
setMotionEndTimeSelector :: Selector
setMotionEndTimeSelector = mkSelector "setMotionEndTime:"

-- | @Selector@ for @motionKeyframeCount@
motionKeyframeCountSelector :: Selector
motionKeyframeCountSelector = mkSelector "motionKeyframeCount"

-- | @Selector@ for @setMotionKeyframeCount:@
setMotionKeyframeCountSelector :: Selector
setMotionKeyframeCountSelector = mkSelector "setMotionKeyframeCount:"

