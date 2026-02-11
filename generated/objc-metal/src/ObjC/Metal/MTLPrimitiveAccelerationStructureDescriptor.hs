{-# LANGUAGE PatternSynonyms #-}
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

-- | @+ descriptor@
descriptor :: IO (Id MTLPrimitiveAccelerationStructureDescriptor)
descriptor  =
  do
    cls' <- getRequiredClass "MTLPrimitiveAccelerationStructureDescriptor"
    sendClassMsg cls' (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Array of geometry descriptors. If motionKeyframeCount is greater than one all geometryDescriptors must be motion versions and have motionKeyframeCount of primitive buffers.
--
-- ObjC selector: @- geometryDescriptors@
geometryDescriptors :: IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor => mtlPrimitiveAccelerationStructureDescriptor -> IO (Id NSArray)
geometryDescriptors mtlPrimitiveAccelerationStructureDescriptor  =
  sendMsg mtlPrimitiveAccelerationStructureDescriptor (mkSelector "geometryDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Array of geometry descriptors. If motionKeyframeCount is greater than one all geometryDescriptors must be motion versions and have motionKeyframeCount of primitive buffers.
--
-- ObjC selector: @- setGeometryDescriptors:@
setGeometryDescriptors :: (IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor, IsNSArray value) => mtlPrimitiveAccelerationStructureDescriptor -> value -> IO ()
setGeometryDescriptors mtlPrimitiveAccelerationStructureDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlPrimitiveAccelerationStructureDescriptor (mkSelector "setGeometryDescriptors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Motion border mode describing what happens if acceleration structure is sampled before motionStartTime. If not set defaults to MTLMotionBorderModeClamp.
--
-- ObjC selector: @- motionStartBorderMode@
motionStartBorderMode :: IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor => mtlPrimitiveAccelerationStructureDescriptor -> IO MTLMotionBorderMode
motionStartBorderMode mtlPrimitiveAccelerationStructureDescriptor  =
  fmap (coerce :: CUInt -> MTLMotionBorderMode) $ sendMsg mtlPrimitiveAccelerationStructureDescriptor (mkSelector "motionStartBorderMode") retCUInt []

-- | Motion border mode describing what happens if acceleration structure is sampled before motionStartTime. If not set defaults to MTLMotionBorderModeClamp.
--
-- ObjC selector: @- setMotionStartBorderMode:@
setMotionStartBorderMode :: IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor => mtlPrimitiveAccelerationStructureDescriptor -> MTLMotionBorderMode -> IO ()
setMotionStartBorderMode mtlPrimitiveAccelerationStructureDescriptor  value =
  sendMsg mtlPrimitiveAccelerationStructureDescriptor (mkSelector "setMotionStartBorderMode:") retVoid [argCUInt (coerce value)]

-- | Motion border mode describing what happens if acceleration structure is sampled after motionEndTime. If not set defaults to MTLMotionBorderModeClamp.
--
-- ObjC selector: @- motionEndBorderMode@
motionEndBorderMode :: IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor => mtlPrimitiveAccelerationStructureDescriptor -> IO MTLMotionBorderMode
motionEndBorderMode mtlPrimitiveAccelerationStructureDescriptor  =
  fmap (coerce :: CUInt -> MTLMotionBorderMode) $ sendMsg mtlPrimitiveAccelerationStructureDescriptor (mkSelector "motionEndBorderMode") retCUInt []

-- | Motion border mode describing what happens if acceleration structure is sampled after motionEndTime. If not set defaults to MTLMotionBorderModeClamp.
--
-- ObjC selector: @- setMotionEndBorderMode:@
setMotionEndBorderMode :: IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor => mtlPrimitiveAccelerationStructureDescriptor -> MTLMotionBorderMode -> IO ()
setMotionEndBorderMode mtlPrimitiveAccelerationStructureDescriptor  value =
  sendMsg mtlPrimitiveAccelerationStructureDescriptor (mkSelector "setMotionEndBorderMode:") retVoid [argCUInt (coerce value)]

-- | Motion start time of this geometry. If not set defaults to 0.0f.
--
-- ObjC selector: @- motionStartTime@
motionStartTime :: IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor => mtlPrimitiveAccelerationStructureDescriptor -> IO CFloat
motionStartTime mtlPrimitiveAccelerationStructureDescriptor  =
  sendMsg mtlPrimitiveAccelerationStructureDescriptor (mkSelector "motionStartTime") retCFloat []

-- | Motion start time of this geometry. If not set defaults to 0.0f.
--
-- ObjC selector: @- setMotionStartTime:@
setMotionStartTime :: IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor => mtlPrimitiveAccelerationStructureDescriptor -> CFloat -> IO ()
setMotionStartTime mtlPrimitiveAccelerationStructureDescriptor  value =
  sendMsg mtlPrimitiveAccelerationStructureDescriptor (mkSelector "setMotionStartTime:") retVoid [argCFloat (fromIntegral value)]

-- | Motion end time of this geometry. If not set defaults to 1.0f.
--
-- ObjC selector: @- motionEndTime@
motionEndTime :: IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor => mtlPrimitiveAccelerationStructureDescriptor -> IO CFloat
motionEndTime mtlPrimitiveAccelerationStructureDescriptor  =
  sendMsg mtlPrimitiveAccelerationStructureDescriptor (mkSelector "motionEndTime") retCFloat []

-- | Motion end time of this geometry. If not set defaults to 1.0f.
--
-- ObjC selector: @- setMotionEndTime:@
setMotionEndTime :: IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor => mtlPrimitiveAccelerationStructureDescriptor -> CFloat -> IO ()
setMotionEndTime mtlPrimitiveAccelerationStructureDescriptor  value =
  sendMsg mtlPrimitiveAccelerationStructureDescriptor (mkSelector "setMotionEndTime:") retVoid [argCFloat (fromIntegral value)]

-- | Motion keyframe count. Is 1 by default which means no motion.
--
-- ObjC selector: @- motionKeyframeCount@
motionKeyframeCount :: IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor => mtlPrimitiveAccelerationStructureDescriptor -> IO CULong
motionKeyframeCount mtlPrimitiveAccelerationStructureDescriptor  =
  sendMsg mtlPrimitiveAccelerationStructureDescriptor (mkSelector "motionKeyframeCount") retCULong []

-- | Motion keyframe count. Is 1 by default which means no motion.
--
-- ObjC selector: @- setMotionKeyframeCount:@
setMotionKeyframeCount :: IsMTLPrimitiveAccelerationStructureDescriptor mtlPrimitiveAccelerationStructureDescriptor => mtlPrimitiveAccelerationStructureDescriptor -> CULong -> IO ()
setMotionKeyframeCount mtlPrimitiveAccelerationStructureDescriptor  value =
  sendMsg mtlPrimitiveAccelerationStructureDescriptor (mkSelector "setMotionKeyframeCount:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

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

