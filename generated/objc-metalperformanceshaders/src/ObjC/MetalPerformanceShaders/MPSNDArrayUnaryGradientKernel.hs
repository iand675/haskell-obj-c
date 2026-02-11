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
  , initWithDeviceSelector
  , initWithDevice_sourceCount_sourceGradientIndexSelector
  , initWithCoder_deviceSelector
  , encodeToCommandBuffer_sourceArray_sourceGradient_gradientStateSelector
  , encodeToCommandBuffer_sourceArray_sourceGradient_gradientState_destinationArraySelector


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

-- | @- initWithDevice:@
initWithDevice :: IsMPSNDArrayUnaryGradientKernel mpsndArrayUnaryGradientKernel => mpsndArrayUnaryGradientKernel -> RawId -> IO (Id MPSNDArrayUnaryGradientKernel)
initWithDevice mpsndArrayUnaryGradientKernel  device =
  sendMsg mpsndArrayUnaryGradientKernel (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDevice:sourceCount:sourceGradientIndex:@
initWithDevice_sourceCount_sourceGradientIndex :: IsMPSNDArrayUnaryGradientKernel mpsndArrayUnaryGradientKernel => mpsndArrayUnaryGradientKernel -> RawId -> CULong -> CULong -> IO (Id MPSNDArrayUnaryGradientKernel)
initWithDevice_sourceCount_sourceGradientIndex mpsndArrayUnaryGradientKernel  device count sourceGradientIndex =
  sendMsg mpsndArrayUnaryGradientKernel (mkSelector "initWithDevice:sourceCount:sourceGradientIndex:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral count), argCULong (fromIntegral sourceGradientIndex)] >>= ownedObject . castPtr

-- | @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNDArrayUnaryGradientKernel mpsndArrayUnaryGradientKernel, IsNSCoder coder) => mpsndArrayUnaryGradientKernel -> coder -> RawId -> IO (Id MPSNDArrayUnaryGradientKernel)
initWithCoder_device mpsndArrayUnaryGradientKernel  coder device =
withObjCPtr coder $ \raw_coder ->
    sendMsg mpsndArrayUnaryGradientKernel (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- encodeToCommandBuffer:sourceArray:sourceGradient:gradientState:@
encodeToCommandBuffer_sourceArray_sourceGradient_gradientState :: (IsMPSNDArrayUnaryGradientKernel mpsndArrayUnaryGradientKernel, IsMPSNDArray sourceArray, IsMPSNDArray gradient, IsMPSState state) => mpsndArrayUnaryGradientKernel -> RawId -> sourceArray -> gradient -> state -> IO (Id MPSNDArray)
encodeToCommandBuffer_sourceArray_sourceGradient_gradientState mpsndArrayUnaryGradientKernel  cmdBuf sourceArray gradient state =
withObjCPtr sourceArray $ \raw_sourceArray ->
  withObjCPtr gradient $ \raw_gradient ->
    withObjCPtr state $ \raw_state ->
        sendMsg mpsndArrayUnaryGradientKernel (mkSelector "encodeToCommandBuffer:sourceArray:sourceGradient:gradientState:") (retPtr retVoid) [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_sourceArray :: Ptr ()), argPtr (castPtr raw_gradient :: Ptr ()), argPtr (castPtr raw_state :: Ptr ())] >>= retainedObject . castPtr

-- | @- encodeToCommandBuffer:sourceArray:sourceGradient:gradientState:destinationArray:@
encodeToCommandBuffer_sourceArray_sourceGradient_gradientState_destinationArray :: (IsMPSNDArrayUnaryGradientKernel mpsndArrayUnaryGradientKernel, IsMPSNDArray sourceArray, IsMPSNDArray gradient, IsMPSState state, IsMPSNDArray destination) => mpsndArrayUnaryGradientKernel -> RawId -> sourceArray -> gradient -> state -> destination -> IO ()
encodeToCommandBuffer_sourceArray_sourceGradient_gradientState_destinationArray mpsndArrayUnaryGradientKernel  cmdBuf sourceArray gradient state destination =
withObjCPtr sourceArray $ \raw_sourceArray ->
  withObjCPtr gradient $ \raw_gradient ->
    withObjCPtr state $ \raw_state ->
      withObjCPtr destination $ \raw_destination ->
          sendMsg mpsndArrayUnaryGradientKernel (mkSelector "encodeToCommandBuffer:sourceArray:sourceGradient:gradientState:destinationArray:") retVoid [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_sourceArray :: Ptr ()), argPtr (castPtr raw_gradient :: Ptr ()), argPtr (castPtr raw_state :: Ptr ()), argPtr (castPtr raw_destination :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:sourceCount:sourceGradientIndex:@
initWithDevice_sourceCount_sourceGradientIndexSelector :: Selector
initWithDevice_sourceCount_sourceGradientIndexSelector = mkSelector "initWithDevice:sourceCount:sourceGradientIndex:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArray:sourceGradient:gradientState:@
encodeToCommandBuffer_sourceArray_sourceGradient_gradientStateSelector :: Selector
encodeToCommandBuffer_sourceArray_sourceGradient_gradientStateSelector = mkSelector "encodeToCommandBuffer:sourceArray:sourceGradient:gradientState:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArray:sourceGradient:gradientState:destinationArray:@
encodeToCommandBuffer_sourceArray_sourceGradient_gradientState_destinationArraySelector :: Selector
encodeToCommandBuffer_sourceArray_sourceGradient_gradientState_destinationArraySelector = mkSelector "encodeToCommandBuffer:sourceArray:sourceGradient:gradientState:destinationArray:"

