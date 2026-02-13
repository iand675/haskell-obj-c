{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSNDArrayUnaryGradientKernel@.
module ObjC.MetalPerformanceShaders.MPSNDArrayUnaryGradientKernel
  ( MPSNDArrayUnaryGradientKernel
  , IsMPSNDArrayUnaryGradientKernel(..)
  , initWithDevice
  , initWithDevice_sourceCount_sourceGradientIndex
  , initWithCoder_device
  , encodeToCommandBuffer_sourceArray_sourceGradient_gradientState
  , encodeToCommandBuffer_sourceArray_sourceGradient_gradientState_destinationArray
  , encodeToCommandBuffer_sourceArray_sourceGradient_gradientStateSelector
  , encodeToCommandBuffer_sourceArray_sourceGradient_gradientState_destinationArraySelector
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

-- | @- initWithDevice:@
initWithDevice :: IsMPSNDArrayUnaryGradientKernel mpsndArrayUnaryGradientKernel => mpsndArrayUnaryGradientKernel -> RawId -> IO (Id MPSNDArrayUnaryGradientKernel)
initWithDevice mpsndArrayUnaryGradientKernel device =
  sendOwnedMessage mpsndArrayUnaryGradientKernel initWithDeviceSelector device

-- | @- initWithDevice:sourceCount:sourceGradientIndex:@
initWithDevice_sourceCount_sourceGradientIndex :: IsMPSNDArrayUnaryGradientKernel mpsndArrayUnaryGradientKernel => mpsndArrayUnaryGradientKernel -> RawId -> CULong -> CULong -> IO (Id MPSNDArrayUnaryGradientKernel)
initWithDevice_sourceCount_sourceGradientIndex mpsndArrayUnaryGradientKernel device count sourceGradientIndex =
  sendOwnedMessage mpsndArrayUnaryGradientKernel initWithDevice_sourceCount_sourceGradientIndexSelector device count sourceGradientIndex

-- | @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNDArrayUnaryGradientKernel mpsndArrayUnaryGradientKernel, IsNSCoder coder) => mpsndArrayUnaryGradientKernel -> coder -> RawId -> IO (Id MPSNDArrayUnaryGradientKernel)
initWithCoder_device mpsndArrayUnaryGradientKernel coder device =
  sendOwnedMessage mpsndArrayUnaryGradientKernel initWithCoder_deviceSelector (toNSCoder coder) device

-- | @- encodeToCommandBuffer:sourceArray:sourceGradient:gradientState:@
encodeToCommandBuffer_sourceArray_sourceGradient_gradientState :: (IsMPSNDArrayUnaryGradientKernel mpsndArrayUnaryGradientKernel, IsMPSNDArray sourceArray, IsMPSNDArray gradient, IsMPSState state) => mpsndArrayUnaryGradientKernel -> RawId -> sourceArray -> gradient -> state -> IO (Id MPSNDArray)
encodeToCommandBuffer_sourceArray_sourceGradient_gradientState mpsndArrayUnaryGradientKernel cmdBuf sourceArray gradient state =
  sendMessage mpsndArrayUnaryGradientKernel encodeToCommandBuffer_sourceArray_sourceGradient_gradientStateSelector cmdBuf (toMPSNDArray sourceArray) (toMPSNDArray gradient) (toMPSState state)

-- | @- encodeToCommandBuffer:sourceArray:sourceGradient:gradientState:destinationArray:@
encodeToCommandBuffer_sourceArray_sourceGradient_gradientState_destinationArray :: (IsMPSNDArrayUnaryGradientKernel mpsndArrayUnaryGradientKernel, IsMPSNDArray sourceArray, IsMPSNDArray gradient, IsMPSState state, IsMPSNDArray destination) => mpsndArrayUnaryGradientKernel -> RawId -> sourceArray -> gradient -> state -> destination -> IO ()
encodeToCommandBuffer_sourceArray_sourceGradient_gradientState_destinationArray mpsndArrayUnaryGradientKernel cmdBuf sourceArray gradient state destination =
  sendMessage mpsndArrayUnaryGradientKernel encodeToCommandBuffer_sourceArray_sourceGradient_gradientState_destinationArraySelector cmdBuf (toMPSNDArray sourceArray) (toMPSNDArray gradient) (toMPSState state) (toMPSNDArray destination)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNDArrayUnaryGradientKernel)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:sourceCount:sourceGradientIndex:@
initWithDevice_sourceCount_sourceGradientIndexSelector :: Selector '[RawId, CULong, CULong] (Id MPSNDArrayUnaryGradientKernel)
initWithDevice_sourceCount_sourceGradientIndexSelector = mkSelector "initWithDevice:sourceCount:sourceGradientIndex:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNDArrayUnaryGradientKernel)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArray:sourceGradient:gradientState:@
encodeToCommandBuffer_sourceArray_sourceGradient_gradientStateSelector :: Selector '[RawId, Id MPSNDArray, Id MPSNDArray, Id MPSState] (Id MPSNDArray)
encodeToCommandBuffer_sourceArray_sourceGradient_gradientStateSelector = mkSelector "encodeToCommandBuffer:sourceArray:sourceGradient:gradientState:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArray:sourceGradient:gradientState:destinationArray:@
encodeToCommandBuffer_sourceArray_sourceGradient_gradientState_destinationArraySelector :: Selector '[RawId, Id MPSNDArray, Id MPSNDArray, Id MPSState, Id MPSNDArray] ()
encodeToCommandBuffer_sourceArray_sourceGradient_gradientState_destinationArraySelector = mkSelector "encodeToCommandBuffer:sourceArray:sourceGradient:gradientState:destinationArray:"

