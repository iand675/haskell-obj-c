{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNDArrayDivisionPrimaryGradient
--
-- This depends on Metal.framework.
--
-- Generated bindings for @MPSNDArrayBinaryPrimaryGradientKernel@.
module ObjC.MetalPerformanceShaders.MPSNDArrayBinaryPrimaryGradientKernel
  ( MPSNDArrayBinaryPrimaryGradientKernel
  , IsMPSNDArrayBinaryPrimaryGradientKernel(..)
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
initWithDevice_sourceCount_sourceGradientIndex :: IsMPSNDArrayBinaryPrimaryGradientKernel mpsndArrayBinaryPrimaryGradientKernel => mpsndArrayBinaryPrimaryGradientKernel -> RawId -> CULong -> CULong -> IO (Id MPSNDArrayBinaryPrimaryGradientKernel)
initWithDevice_sourceCount_sourceGradientIndex mpsndArrayBinaryPrimaryGradientKernel device count sourceGradientIndex =
  sendOwnedMessage mpsndArrayBinaryPrimaryGradientKernel initWithDevice_sourceCount_sourceGradientIndexSelector device count sourceGradientIndex

-- | @- initWithDevice:@
initWithDevice :: IsMPSNDArrayBinaryPrimaryGradientKernel mpsndArrayBinaryPrimaryGradientKernel => mpsndArrayBinaryPrimaryGradientKernel -> RawId -> IO (Id MPSNDArrayBinaryPrimaryGradientKernel)
initWithDevice mpsndArrayBinaryPrimaryGradientKernel device =
  sendOwnedMessage mpsndArrayBinaryPrimaryGradientKernel initWithDeviceSelector device

-- | @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNDArrayBinaryPrimaryGradientKernel mpsndArrayBinaryPrimaryGradientKernel, IsNSCoder coder) => mpsndArrayBinaryPrimaryGradientKernel -> coder -> RawId -> IO (Id MPSNDArrayBinaryPrimaryGradientKernel)
initWithCoder_device mpsndArrayBinaryPrimaryGradientKernel coder device =
  sendOwnedMessage mpsndArrayBinaryPrimaryGradientKernel initWithCoder_deviceSelector (toNSCoder coder) device

-- | @- encodeToCommandBuffer:primarySourceArray:secondarySourceArray:sourceGradient:gradientState:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState :: (IsMPSNDArrayBinaryPrimaryGradientKernel mpsndArrayBinaryPrimaryGradientKernel, IsMPSNDArray primarySourceArray, IsMPSNDArray secondarySourceArray, IsMPSNDArray gradient, IsMPSState state) => mpsndArrayBinaryPrimaryGradientKernel -> RawId -> primarySourceArray -> secondarySourceArray -> gradient -> state -> IO (Id MPSNDArray)
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState mpsndArrayBinaryPrimaryGradientKernel cmdBuf primarySourceArray secondarySourceArray gradient state =
  sendMessage mpsndArrayBinaryPrimaryGradientKernel encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientStateSelector cmdBuf (toMPSNDArray primarySourceArray) (toMPSNDArray secondarySourceArray) (toMPSNDArray gradient) (toMPSState state)

-- | @- encodeToCommandBuffer:primarySourceArray:secondarySourceArray:sourceGradient:gradientState:destinationArray:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState_destinationArray :: (IsMPSNDArrayBinaryPrimaryGradientKernel mpsndArrayBinaryPrimaryGradientKernel, IsMPSNDArray primarySourceArray, IsMPSNDArray secondarySourceArray, IsMPSNDArray gradient, IsMPSState state, IsMPSNDArray destination) => mpsndArrayBinaryPrimaryGradientKernel -> RawId -> primarySourceArray -> secondarySourceArray -> gradient -> state -> destination -> IO ()
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState_destinationArray mpsndArrayBinaryPrimaryGradientKernel cmdBuf primarySourceArray secondarySourceArray gradient state destination =
  sendMessage mpsndArrayBinaryPrimaryGradientKernel encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState_destinationArraySelector cmdBuf (toMPSNDArray primarySourceArray) (toMPSNDArray secondarySourceArray) (toMPSNDArray gradient) (toMPSState state) (toMPSNDArray destination)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:sourceCount:sourceGradientIndex:@
initWithDevice_sourceCount_sourceGradientIndexSelector :: Selector '[RawId, CULong, CULong] (Id MPSNDArrayBinaryPrimaryGradientKernel)
initWithDevice_sourceCount_sourceGradientIndexSelector = mkSelector "initWithDevice:sourceCount:sourceGradientIndex:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNDArrayBinaryPrimaryGradientKernel)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNDArrayBinaryPrimaryGradientKernel)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:primarySourceArray:secondarySourceArray:sourceGradient:gradientState:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientStateSelector :: Selector '[RawId, Id MPSNDArray, Id MPSNDArray, Id MPSNDArray, Id MPSState] (Id MPSNDArray)
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientStateSelector = mkSelector "encodeToCommandBuffer:primarySourceArray:secondarySourceArray:sourceGradient:gradientState:"

-- | @Selector@ for @encodeToCommandBuffer:primarySourceArray:secondarySourceArray:sourceGradient:gradientState:destinationArray:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState_destinationArraySelector :: Selector '[RawId, Id MPSNDArray, Id MPSNDArray, Id MPSNDArray, Id MPSState, Id MPSNDArray] ()
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState_destinationArraySelector = mkSelector "encodeToCommandBuffer:primarySourceArray:secondarySourceArray:sourceGradient:gradientState:destinationArray:"

