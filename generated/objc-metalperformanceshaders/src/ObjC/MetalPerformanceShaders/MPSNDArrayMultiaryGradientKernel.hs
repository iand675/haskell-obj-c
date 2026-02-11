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
  , initWithDevice_sourceCountSelector
  , initWithCoder_deviceSelector
  , initWithDevice_sourceCount_sourceGradientIndexSelector
  , encodeToCommandBuffer_sourceArrays_sourceGradient_gradientStateSelector
  , encodeToCommandBuffer_sourceArrays_sourceGradient_gradientState_destinationArraySelector


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

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:sourceCount:@
initWithDevice_sourceCount :: IsMPSNDArrayMultiaryGradientKernel mpsndArrayMultiaryGradientKernel => mpsndArrayMultiaryGradientKernel -> RawId -> CULong -> IO (Id MPSNDArrayMultiaryGradientKernel)
initWithDevice_sourceCount mpsndArrayMultiaryGradientKernel  device count =
  sendMsg mpsndArrayMultiaryGradientKernel (mkSelector "initWithDevice:sourceCount:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral count)] >>= ownedObject . castPtr

-- | @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNDArrayMultiaryGradientKernel mpsndArrayMultiaryGradientKernel, IsNSCoder coder) => mpsndArrayMultiaryGradientKernel -> coder -> RawId -> IO (Id MPSNDArrayMultiaryGradientKernel)
initWithCoder_device mpsndArrayMultiaryGradientKernel  coder device =
withObjCPtr coder $ \raw_coder ->
    sendMsg mpsndArrayMultiaryGradientKernel (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
initWithDevice_sourceCount_sourceGradientIndex mpsndArrayMultiaryGradientKernel  device count sourceGradientIndex =
  sendMsg mpsndArrayMultiaryGradientKernel (mkSelector "initWithDevice:sourceCount:sourceGradientIndex:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral count), argCULong (fromIntegral sourceGradientIndex)] >>= ownedObject . castPtr

-- | @- encodeToCommandBuffer:sourceArrays:sourceGradient:gradientState:@
encodeToCommandBuffer_sourceArrays_sourceGradient_gradientState :: (IsMPSNDArrayMultiaryGradientKernel mpsndArrayMultiaryGradientKernel, IsNSArray sources, IsMPSNDArray gradient, IsMPSState state) => mpsndArrayMultiaryGradientKernel -> RawId -> sources -> gradient -> state -> IO (Id MPSNDArray)
encodeToCommandBuffer_sourceArrays_sourceGradient_gradientState mpsndArrayMultiaryGradientKernel  cmdBuf sources gradient state =
withObjCPtr sources $ \raw_sources ->
  withObjCPtr gradient $ \raw_gradient ->
    withObjCPtr state $ \raw_state ->
        sendMsg mpsndArrayMultiaryGradientKernel (mkSelector "encodeToCommandBuffer:sourceArrays:sourceGradient:gradientState:") (retPtr retVoid) [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_sources :: Ptr ()), argPtr (castPtr raw_gradient :: Ptr ()), argPtr (castPtr raw_state :: Ptr ())] >>= retainedObject . castPtr

-- | @- encodeToCommandBuffer:sourceArrays:sourceGradient:gradientState:destinationArray:@
encodeToCommandBuffer_sourceArrays_sourceGradient_gradientState_destinationArray :: (IsMPSNDArrayMultiaryGradientKernel mpsndArrayMultiaryGradientKernel, IsNSArray sources, IsMPSNDArray gradient, IsMPSState state, IsMPSNDArray destination) => mpsndArrayMultiaryGradientKernel -> RawId -> sources -> gradient -> state -> destination -> IO ()
encodeToCommandBuffer_sourceArrays_sourceGradient_gradientState_destinationArray mpsndArrayMultiaryGradientKernel  cmdBuf sources gradient state destination =
withObjCPtr sources $ \raw_sources ->
  withObjCPtr gradient $ \raw_gradient ->
    withObjCPtr state $ \raw_state ->
      withObjCPtr destination $ \raw_destination ->
          sendMsg mpsndArrayMultiaryGradientKernel (mkSelector "encodeToCommandBuffer:sourceArrays:sourceGradient:gradientState:destinationArray:") retVoid [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_sources :: Ptr ()), argPtr (castPtr raw_gradient :: Ptr ()), argPtr (castPtr raw_state :: Ptr ()), argPtr (castPtr raw_destination :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:sourceCount:@
initWithDevice_sourceCountSelector :: Selector
initWithDevice_sourceCountSelector = mkSelector "initWithDevice:sourceCount:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:sourceCount:sourceGradientIndex:@
initWithDevice_sourceCount_sourceGradientIndexSelector :: Selector
initWithDevice_sourceCount_sourceGradientIndexSelector = mkSelector "initWithDevice:sourceCount:sourceGradientIndex:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArrays:sourceGradient:gradientState:@
encodeToCommandBuffer_sourceArrays_sourceGradient_gradientStateSelector :: Selector
encodeToCommandBuffer_sourceArrays_sourceGradient_gradientStateSelector = mkSelector "encodeToCommandBuffer:sourceArrays:sourceGradient:gradientState:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArrays:sourceGradient:gradientState:destinationArray:@
encodeToCommandBuffer_sourceArrays_sourceGradient_gradientState_destinationArraySelector :: Selector
encodeToCommandBuffer_sourceArrays_sourceGradient_gradientState_destinationArraySelector = mkSelector "encodeToCommandBuffer:sourceArrays:sourceGradient:gradientState:destinationArray:"

