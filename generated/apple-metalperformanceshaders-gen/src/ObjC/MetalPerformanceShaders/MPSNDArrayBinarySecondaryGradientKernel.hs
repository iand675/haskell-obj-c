{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNDArrayDivisionSecondaryGradient
--
-- This depends on Metal.framework.
--
-- Generated bindings for @MPSNDArrayBinarySecondaryGradientKernel@.
module ObjC.MetalPerformanceShaders.MPSNDArrayBinarySecondaryGradientKernel
  ( MPSNDArrayBinarySecondaryGradientKernel
  , IsMPSNDArrayBinarySecondaryGradientKernel(..)
  , initWithDevice_sourceCount_sourceGradientIndex
  , initWithDevice
  , initWithCoder_device
  , encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState
  , encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState_destinationArray
  , encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientStateSelector
  , encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState_destinationArraySelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_sourceCount_sourceGradientIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:sourceCount:sourceGradientIndex:@
initWithDevice_sourceCount_sourceGradientIndex :: IsMPSNDArrayBinarySecondaryGradientKernel mpsndArrayBinarySecondaryGradientKernel => mpsndArrayBinarySecondaryGradientKernel -> RawId -> CULong -> CULong -> IO (Id MPSNDArrayBinarySecondaryGradientKernel)
initWithDevice_sourceCount_sourceGradientIndex mpsndArrayBinarySecondaryGradientKernel device count sourceGradientIndex =
  sendOwnedMessage mpsndArrayBinarySecondaryGradientKernel initWithDevice_sourceCount_sourceGradientIndexSelector device count sourceGradientIndex

-- | @- initWithDevice:@
initWithDevice :: IsMPSNDArrayBinarySecondaryGradientKernel mpsndArrayBinarySecondaryGradientKernel => mpsndArrayBinarySecondaryGradientKernel -> RawId -> IO (Id MPSNDArrayBinarySecondaryGradientKernel)
initWithDevice mpsndArrayBinarySecondaryGradientKernel device =
  sendOwnedMessage mpsndArrayBinarySecondaryGradientKernel initWithDeviceSelector device

-- | @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNDArrayBinarySecondaryGradientKernel mpsndArrayBinarySecondaryGradientKernel, IsNSCoder coder) => mpsndArrayBinarySecondaryGradientKernel -> coder -> RawId -> IO (Id MPSNDArrayBinarySecondaryGradientKernel)
initWithCoder_device mpsndArrayBinarySecondaryGradientKernel coder device =
  sendOwnedMessage mpsndArrayBinarySecondaryGradientKernel initWithCoder_deviceSelector (toNSCoder coder) device

-- | @- encodeToCommandBuffer:primarySourceArray:secondarySourceArray:sourceGradient:gradientState:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState :: (IsMPSNDArrayBinarySecondaryGradientKernel mpsndArrayBinarySecondaryGradientKernel, IsMPSNDArray primarySourceArray, IsMPSNDArray secondarySourceArray, IsMPSNDArray gradient, IsMPSState state) => mpsndArrayBinarySecondaryGradientKernel -> RawId -> primarySourceArray -> secondarySourceArray -> gradient -> state -> IO (Id MPSNDArray)
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState mpsndArrayBinarySecondaryGradientKernel cmdBuf primarySourceArray secondarySourceArray gradient state =
  sendMessage mpsndArrayBinarySecondaryGradientKernel encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientStateSelector cmdBuf (toMPSNDArray primarySourceArray) (toMPSNDArray secondarySourceArray) (toMPSNDArray gradient) (toMPSState state)

-- | @- encodeToCommandBuffer:primarySourceArray:secondarySourceArray:sourceGradient:gradientState:destinationArray:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState_destinationArray :: (IsMPSNDArrayBinarySecondaryGradientKernel mpsndArrayBinarySecondaryGradientKernel, IsMPSNDArray primarySourceArray, IsMPSNDArray secondarySourceArray, IsMPSNDArray gradient, IsMPSState state, IsMPSNDArray destination) => mpsndArrayBinarySecondaryGradientKernel -> RawId -> primarySourceArray -> secondarySourceArray -> gradient -> state -> destination -> IO ()
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState_destinationArray mpsndArrayBinarySecondaryGradientKernel cmdBuf primarySourceArray secondarySourceArray gradient state destination =
  sendMessage mpsndArrayBinarySecondaryGradientKernel encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState_destinationArraySelector cmdBuf (toMPSNDArray primarySourceArray) (toMPSNDArray secondarySourceArray) (toMPSNDArray gradient) (toMPSState state) (toMPSNDArray destination)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:sourceCount:sourceGradientIndex:@
initWithDevice_sourceCount_sourceGradientIndexSelector :: Selector '[RawId, CULong, CULong] (Id MPSNDArrayBinarySecondaryGradientKernel)
initWithDevice_sourceCount_sourceGradientIndexSelector = mkSelector "initWithDevice:sourceCount:sourceGradientIndex:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNDArrayBinarySecondaryGradientKernel)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNDArrayBinarySecondaryGradientKernel)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:primarySourceArray:secondarySourceArray:sourceGradient:gradientState:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientStateSelector :: Selector '[RawId, Id MPSNDArray, Id MPSNDArray, Id MPSNDArray, Id MPSState] (Id MPSNDArray)
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientStateSelector = mkSelector "encodeToCommandBuffer:primarySourceArray:secondarySourceArray:sourceGradient:gradientState:"

-- | @Selector@ for @encodeToCommandBuffer:primarySourceArray:secondarySourceArray:sourceGradient:gradientState:destinationArray:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState_destinationArraySelector :: Selector '[RawId, Id MPSNDArray, Id MPSNDArray, Id MPSNDArray, Id MPSState, Id MPSNDArray] ()
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState_destinationArraySelector = mkSelector "encodeToCommandBuffer:primarySourceArray:secondarySourceArray:sourceGradient:gradientState:destinationArray:"

