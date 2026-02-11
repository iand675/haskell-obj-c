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
  , initWithDevice_sourceCount_sourceGradientIndexSelector
  , initWithDeviceSelector
  , initWithCoder_deviceSelector
  , encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientStateSelector
  , encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState_destinationArraySelector


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

-- | @- initWithDevice:sourceCount:sourceGradientIndex:@
initWithDevice_sourceCount_sourceGradientIndex :: IsMPSNDArrayBinarySecondaryGradientKernel mpsndArrayBinarySecondaryGradientKernel => mpsndArrayBinarySecondaryGradientKernel -> RawId -> CULong -> CULong -> IO (Id MPSNDArrayBinarySecondaryGradientKernel)
initWithDevice_sourceCount_sourceGradientIndex mpsndArrayBinarySecondaryGradientKernel  device count sourceGradientIndex =
  sendMsg mpsndArrayBinarySecondaryGradientKernel (mkSelector "initWithDevice:sourceCount:sourceGradientIndex:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral count), argCULong (fromIntegral sourceGradientIndex)] >>= ownedObject . castPtr

-- | @- initWithDevice:@
initWithDevice :: IsMPSNDArrayBinarySecondaryGradientKernel mpsndArrayBinarySecondaryGradientKernel => mpsndArrayBinarySecondaryGradientKernel -> RawId -> IO (Id MPSNDArrayBinarySecondaryGradientKernel)
initWithDevice mpsndArrayBinarySecondaryGradientKernel  device =
  sendMsg mpsndArrayBinarySecondaryGradientKernel (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNDArrayBinarySecondaryGradientKernel mpsndArrayBinarySecondaryGradientKernel, IsNSCoder coder) => mpsndArrayBinarySecondaryGradientKernel -> coder -> RawId -> IO (Id MPSNDArrayBinarySecondaryGradientKernel)
initWithCoder_device mpsndArrayBinarySecondaryGradientKernel  coder device =
withObjCPtr coder $ \raw_coder ->
    sendMsg mpsndArrayBinarySecondaryGradientKernel (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- encodeToCommandBuffer:primarySourceArray:secondarySourceArray:sourceGradient:gradientState:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState :: (IsMPSNDArrayBinarySecondaryGradientKernel mpsndArrayBinarySecondaryGradientKernel, IsMPSNDArray primarySourceArray, IsMPSNDArray secondarySourceArray, IsMPSNDArray gradient, IsMPSState state) => mpsndArrayBinarySecondaryGradientKernel -> RawId -> primarySourceArray -> secondarySourceArray -> gradient -> state -> IO (Id MPSNDArray)
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState mpsndArrayBinarySecondaryGradientKernel  cmdBuf primarySourceArray secondarySourceArray gradient state =
withObjCPtr primarySourceArray $ \raw_primarySourceArray ->
  withObjCPtr secondarySourceArray $ \raw_secondarySourceArray ->
    withObjCPtr gradient $ \raw_gradient ->
      withObjCPtr state $ \raw_state ->
          sendMsg mpsndArrayBinarySecondaryGradientKernel (mkSelector "encodeToCommandBuffer:primarySourceArray:secondarySourceArray:sourceGradient:gradientState:") (retPtr retVoid) [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_primarySourceArray :: Ptr ()), argPtr (castPtr raw_secondarySourceArray :: Ptr ()), argPtr (castPtr raw_gradient :: Ptr ()), argPtr (castPtr raw_state :: Ptr ())] >>= retainedObject . castPtr

-- | @- encodeToCommandBuffer:primarySourceArray:secondarySourceArray:sourceGradient:gradientState:destinationArray:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState_destinationArray :: (IsMPSNDArrayBinarySecondaryGradientKernel mpsndArrayBinarySecondaryGradientKernel, IsMPSNDArray primarySourceArray, IsMPSNDArray secondarySourceArray, IsMPSNDArray gradient, IsMPSState state, IsMPSNDArray destination) => mpsndArrayBinarySecondaryGradientKernel -> RawId -> primarySourceArray -> secondarySourceArray -> gradient -> state -> destination -> IO ()
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState_destinationArray mpsndArrayBinarySecondaryGradientKernel  cmdBuf primarySourceArray secondarySourceArray gradient state destination =
withObjCPtr primarySourceArray $ \raw_primarySourceArray ->
  withObjCPtr secondarySourceArray $ \raw_secondarySourceArray ->
    withObjCPtr gradient $ \raw_gradient ->
      withObjCPtr state $ \raw_state ->
        withObjCPtr destination $ \raw_destination ->
            sendMsg mpsndArrayBinarySecondaryGradientKernel (mkSelector "encodeToCommandBuffer:primarySourceArray:secondarySourceArray:sourceGradient:gradientState:destinationArray:") retVoid [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_primarySourceArray :: Ptr ()), argPtr (castPtr raw_secondarySourceArray :: Ptr ()), argPtr (castPtr raw_gradient :: Ptr ()), argPtr (castPtr raw_state :: Ptr ()), argPtr (castPtr raw_destination :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:sourceCount:sourceGradientIndex:@
initWithDevice_sourceCount_sourceGradientIndexSelector :: Selector
initWithDevice_sourceCount_sourceGradientIndexSelector = mkSelector "initWithDevice:sourceCount:sourceGradientIndex:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:primarySourceArray:secondarySourceArray:sourceGradient:gradientState:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientStateSelector :: Selector
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientStateSelector = mkSelector "encodeToCommandBuffer:primarySourceArray:secondarySourceArray:sourceGradient:gradientState:"

-- | @Selector@ for @encodeToCommandBuffer:primarySourceArray:secondarySourceArray:sourceGradient:gradientState:destinationArray:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState_destinationArraySelector :: Selector
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_sourceGradient_gradientState_destinationArraySelector = mkSelector "encodeToCommandBuffer:primarySourceArray:secondarySourceArray:sourceGradient:gradientState:destinationArray:"

