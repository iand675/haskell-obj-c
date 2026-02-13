{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSNDArrayMultiaryGradientKernel@.
module ObjC.MetalPerformanceShaders.MPSNDArrayMultiaryGradientKernel
  ( MPSNDArrayMultiaryGradientKernel
  , IsMPSNDArrayMultiaryGradientKernel(..)
  , initWithDevice_sourceCount
  , initWithCoder_device
  , initWithDevice_sourceCount_sourceGradientIndex
  , encodeToCommandBuffer_sourceArrays_sourceGradient_gradientState
  , encodeToCommandBuffer_sourceArrays_sourceGradient_gradientState_destinationArray
  , encodeToCommandBuffer_sourceArrays_sourceGradient_gradientStateSelector
  , encodeToCommandBuffer_sourceArrays_sourceGradient_gradientState_destinationArraySelector
  , initWithCoder_deviceSelector
  , initWithDevice_sourceCountSelector
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

-- | @- initWithDevice:sourceCount:@
initWithDevice_sourceCount :: IsMPSNDArrayMultiaryGradientKernel mpsndArrayMultiaryGradientKernel => mpsndArrayMultiaryGradientKernel -> RawId -> CULong -> IO (Id MPSNDArrayMultiaryGradientKernel)
initWithDevice_sourceCount mpsndArrayMultiaryGradientKernel device count =
  sendOwnedMessage mpsndArrayMultiaryGradientKernel initWithDevice_sourceCountSelector device count

-- | @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNDArrayMultiaryGradientKernel mpsndArrayMultiaryGradientKernel, IsNSCoder coder) => mpsndArrayMultiaryGradientKernel -> coder -> RawId -> IO (Id MPSNDArrayMultiaryGradientKernel)
initWithCoder_device mpsndArrayMultiaryGradientKernel coder device =
  sendOwnedMessage mpsndArrayMultiaryGradientKernel initWithCoder_deviceSelector (toNSCoder coder) device

-- | Initialize a MPSNDArrayMultiaryKernel
--
-- @device@ — The device on which the kernel will run
--
-- @count@ — The maximum number of NDArrays read by the kernel
--
-- @sourceGradientIndex@ — The source index for which gradient will be calculated
--
-- Returns: A valid MPSNDArrayMultiaryKernel, or nil if allocation failure.
--
-- ObjC selector: @- initWithDevice:sourceCount:sourceGradientIndex:@
initWithDevice_sourceCount_sourceGradientIndex :: IsMPSNDArrayMultiaryGradientKernel mpsndArrayMultiaryGradientKernel => mpsndArrayMultiaryGradientKernel -> RawId -> CULong -> CULong -> IO (Id MPSNDArrayMultiaryGradientKernel)
initWithDevice_sourceCount_sourceGradientIndex mpsndArrayMultiaryGradientKernel device count sourceGradientIndex =
  sendOwnedMessage mpsndArrayMultiaryGradientKernel initWithDevice_sourceCount_sourceGradientIndexSelector device count sourceGradientIndex

-- | @- encodeToCommandBuffer:sourceArrays:sourceGradient:gradientState:@
encodeToCommandBuffer_sourceArrays_sourceGradient_gradientState :: (IsMPSNDArrayMultiaryGradientKernel mpsndArrayMultiaryGradientKernel, IsNSArray sources, IsMPSNDArray gradient, IsMPSState state) => mpsndArrayMultiaryGradientKernel -> RawId -> sources -> gradient -> state -> IO (Id MPSNDArray)
encodeToCommandBuffer_sourceArrays_sourceGradient_gradientState mpsndArrayMultiaryGradientKernel cmdBuf sources gradient state =
  sendMessage mpsndArrayMultiaryGradientKernel encodeToCommandBuffer_sourceArrays_sourceGradient_gradientStateSelector cmdBuf (toNSArray sources) (toMPSNDArray gradient) (toMPSState state)

-- | @- encodeToCommandBuffer:sourceArrays:sourceGradient:gradientState:destinationArray:@
encodeToCommandBuffer_sourceArrays_sourceGradient_gradientState_destinationArray :: (IsMPSNDArrayMultiaryGradientKernel mpsndArrayMultiaryGradientKernel, IsNSArray sources, IsMPSNDArray gradient, IsMPSState state, IsMPSNDArray destination) => mpsndArrayMultiaryGradientKernel -> RawId -> sources -> gradient -> state -> destination -> IO ()
encodeToCommandBuffer_sourceArrays_sourceGradient_gradientState_destinationArray mpsndArrayMultiaryGradientKernel cmdBuf sources gradient state destination =
  sendMessage mpsndArrayMultiaryGradientKernel encodeToCommandBuffer_sourceArrays_sourceGradient_gradientState_destinationArraySelector cmdBuf (toNSArray sources) (toMPSNDArray gradient) (toMPSState state) (toMPSNDArray destination)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:sourceCount:@
initWithDevice_sourceCountSelector :: Selector '[RawId, CULong] (Id MPSNDArrayMultiaryGradientKernel)
initWithDevice_sourceCountSelector = mkSelector "initWithDevice:sourceCount:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNDArrayMultiaryGradientKernel)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:sourceCount:sourceGradientIndex:@
initWithDevice_sourceCount_sourceGradientIndexSelector :: Selector '[RawId, CULong, CULong] (Id MPSNDArrayMultiaryGradientKernel)
initWithDevice_sourceCount_sourceGradientIndexSelector = mkSelector "initWithDevice:sourceCount:sourceGradientIndex:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArrays:sourceGradient:gradientState:@
encodeToCommandBuffer_sourceArrays_sourceGradient_gradientStateSelector :: Selector '[RawId, Id NSArray, Id MPSNDArray, Id MPSState] (Id MPSNDArray)
encodeToCommandBuffer_sourceArrays_sourceGradient_gradientStateSelector = mkSelector "encodeToCommandBuffer:sourceArrays:sourceGradient:gradientState:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArrays:sourceGradient:gradientState:destinationArray:@
encodeToCommandBuffer_sourceArrays_sourceGradient_gradientState_destinationArraySelector :: Selector '[RawId, Id NSArray, Id MPSNDArray, Id MPSState, Id MPSNDArray] ()
encodeToCommandBuffer_sourceArrays_sourceGradient_gradientState_destinationArraySelector = mkSelector "encodeToCommandBuffer:sourceArrays:sourceGradient:gradientState:destinationArray:"

