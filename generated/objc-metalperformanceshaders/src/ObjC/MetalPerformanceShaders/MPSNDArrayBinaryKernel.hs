{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSNDArrayBinaryKernel@.
module ObjC.MetalPerformanceShaders.MPSNDArrayBinaryKernel
  ( MPSNDArrayBinaryKernel
  , IsMPSNDArrayBinaryKernel(..)
  , initWithDevice
  , initWithDevice_sourceCount
  , initWithCoder_device
  , encodeToCommandBuffer_primarySourceArray_secondarySourceArray
  , encodeToCommandBuffer_primarySourceArray_secondarySourceArray_destinationArray
  , encodeToCommandBuffer_primarySourceArray_secondarySourceArray_resultState_outputStateIsTemporary
  , encodeToCommandBuffer_primarySourceArray_secondarySourceArray_resultState_destinationArray
  , primaryEdgeMode
  , secondaryEdgeMode
  , initWithDeviceSelector
  , initWithDevice_sourceCountSelector
  , initWithCoder_deviceSelector
  , encodeToCommandBuffer_primarySourceArray_secondarySourceArraySelector
  , encodeToCommandBuffer_primarySourceArray_secondarySourceArray_destinationArraySelector
  , encodeToCommandBuffer_primarySourceArray_secondarySourceArray_resultState_outputStateIsTemporarySelector
  , encodeToCommandBuffer_primarySourceArray_secondarySourceArray_resultState_destinationArraySelector
  , primaryEdgeModeSelector
  , secondaryEdgeModeSelector

  -- * Enum types
  , MPSImageEdgeMode(MPSImageEdgeMode)
  , pattern MPSImageEdgeModeZero
  , pattern MPSImageEdgeModeClamp
  , pattern MPSImageEdgeModeMirror
  , pattern MPSImageEdgeModeMirrorWithEdge
  , pattern MPSImageEdgeModeConstant

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
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:@
initWithDevice :: IsMPSNDArrayBinaryKernel mpsndArrayBinaryKernel => mpsndArrayBinaryKernel -> RawId -> IO (Id MPSNDArrayBinaryKernel)
initWithDevice mpsndArrayBinaryKernel  device =
  sendMsg mpsndArrayBinaryKernel (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDevice:sourceCount:@
initWithDevice_sourceCount :: IsMPSNDArrayBinaryKernel mpsndArrayBinaryKernel => mpsndArrayBinaryKernel -> RawId -> CULong -> IO (Id MPSNDArrayBinaryKernel)
initWithDevice_sourceCount mpsndArrayBinaryKernel  device count =
  sendMsg mpsndArrayBinaryKernel (mkSelector "initWithDevice:sourceCount:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral count)] >>= ownedObject . castPtr

-- | @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNDArrayBinaryKernel mpsndArrayBinaryKernel, IsNSCoder coder) => mpsndArrayBinaryKernel -> coder -> RawId -> IO (Id MPSNDArrayBinaryKernel)
initWithCoder_device mpsndArrayBinaryKernel  coder device =
withObjCPtr coder $ \raw_coder ->
    sendMsg mpsndArrayBinaryKernel (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Encode a simple inference NDArray kernel and return a NDArray to hold the result
--
-- @cmdBuf@ — The command buffer into which to encode the kernel
--
-- @primarySourceArray@ — The primary source for the filter in an NSArray.
--
-- @secondarySourceArray@ — The secondary source for the filter in an NSArray.
--
-- Returns: A newly allocated MPSNDArray that will contain the result of the calculation              when the command buffer completes successfully.
--
-- ObjC selector: @- encodeToCommandBuffer:primarySourceArray:secondarySourceArray:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArray :: (IsMPSNDArrayBinaryKernel mpsndArrayBinaryKernel, IsMPSNDArray primarySourceArray, IsMPSNDArray secondarySourceArray) => mpsndArrayBinaryKernel -> RawId -> primarySourceArray -> secondarySourceArray -> IO (Id MPSNDArray)
encodeToCommandBuffer_primarySourceArray_secondarySourceArray mpsndArrayBinaryKernel  cmdBuf primarySourceArray secondarySourceArray =
withObjCPtr primarySourceArray $ \raw_primarySourceArray ->
  withObjCPtr secondarySourceArray $ \raw_secondarySourceArray ->
      sendMsg mpsndArrayBinaryKernel (mkSelector "encodeToCommandBuffer:primarySourceArray:secondarySourceArray:") (retPtr retVoid) [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_primarySourceArray :: Ptr ()), argPtr (castPtr raw_secondarySourceArray :: Ptr ())] >>= retainedObject . castPtr

-- | Encode a simple inference NDArray kernel and return a NDArray to hold the result
--
-- @cmdBuf@ — The command buffer into which to encode the kernel
--
-- @primarySourceArray@ — The primary source for the filter in an NSArray.
--
-- @secondarySourceArray@ — The secondary source for the filter in an NSArray.
--
-- @destination@ — The NDArray to receive the result
--
-- ObjC selector: @- encodeToCommandBuffer:primarySourceArray:secondarySourceArray:destinationArray:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_destinationArray :: (IsMPSNDArrayBinaryKernel mpsndArrayBinaryKernel, IsMPSNDArray primarySourceArray, IsMPSNDArray secondarySourceArray, IsMPSNDArray destination) => mpsndArrayBinaryKernel -> RawId -> primarySourceArray -> secondarySourceArray -> destination -> IO ()
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_destinationArray mpsndArrayBinaryKernel  cmdBuf primarySourceArray secondarySourceArray destination =
withObjCPtr primarySourceArray $ \raw_primarySourceArray ->
  withObjCPtr secondarySourceArray $ \raw_secondarySourceArray ->
    withObjCPtr destination $ \raw_destination ->
        sendMsg mpsndArrayBinaryKernel (mkSelector "encodeToCommandBuffer:primarySourceArray:secondarySourceArray:destinationArray:") retVoid [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_primarySourceArray :: Ptr ()), argPtr (castPtr raw_secondarySourceArray :: Ptr ()), argPtr (castPtr raw_destination :: Ptr ())]

-- | Encode a simple inference NDArray kernel and return a NDArray to hold the result
--
-- @cmdBuf@ — The command buffer into which to encode the kernel
--
-- @primarySourceArray@ — The primary source for the filter in an NSArray.
--
-- @secondarySourceArray@ — The secondary source for the filter in an NSArray.
--
-- @outGradientState@ — If non-nil, the address output gradient state is written to this address
--
-- @outputStateIsTemporary@ — If YES, the state if any will be allocated to contain temporary textures and buffers as needed
--
-- Returns: A newly allocated MPSNDArray that will contain the result of the calculation              when the command buffer completes successfully.
--
-- ObjC selector: @- encodeToCommandBuffer:primarySourceArray:secondarySourceArray:resultState:outputStateIsTemporary:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_resultState_outputStateIsTemporary :: (IsMPSNDArrayBinaryKernel mpsndArrayBinaryKernel, IsMPSNDArray primarySourceArray, IsMPSNDArray secondarySourceArray, IsMPSState outGradientState) => mpsndArrayBinaryKernel -> RawId -> primarySourceArray -> secondarySourceArray -> outGradientState -> Bool -> IO (Id MPSNDArray)
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_resultState_outputStateIsTemporary mpsndArrayBinaryKernel  cmdBuf primarySourceArray secondarySourceArray outGradientState outputStateIsTemporary =
withObjCPtr primarySourceArray $ \raw_primarySourceArray ->
  withObjCPtr secondarySourceArray $ \raw_secondarySourceArray ->
    withObjCPtr outGradientState $ \raw_outGradientState ->
        sendMsg mpsndArrayBinaryKernel (mkSelector "encodeToCommandBuffer:primarySourceArray:secondarySourceArray:resultState:outputStateIsTemporary:") (retPtr retVoid) [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_primarySourceArray :: Ptr ()), argPtr (castPtr raw_secondarySourceArray :: Ptr ()), argPtr (castPtr raw_outGradientState :: Ptr ()), argCULong (if outputStateIsTemporary then 1 else 0)] >>= retainedObject . castPtr

-- | Encode a simple inference NDArray kernel and return a NDArray to hold the result
--
-- @cmdBuf@ — The command buffer into which to encode the kernel
--
-- @primarySourceArray@ — The primary source for the filter in an NSArray.
--
-- @secondarySourceArray@ — The secondary source for the filter in an NSArray.
--
-- @outGradientState@ — The output gradient state to record the operation for later use by gradient
--
-- @destination@ — A destination array to contain the result of the calculation              when the command buffer completes successfully.
--
-- ObjC selector: @- encodeToCommandBuffer:primarySourceArray:secondarySourceArray:resultState:destinationArray:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_resultState_destinationArray :: (IsMPSNDArrayBinaryKernel mpsndArrayBinaryKernel, IsMPSNDArray primarySourceArray, IsMPSNDArray secondarySourceArray, IsMPSState outGradientState, IsMPSNDArray destination) => mpsndArrayBinaryKernel -> RawId -> primarySourceArray -> secondarySourceArray -> outGradientState -> destination -> IO ()
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_resultState_destinationArray mpsndArrayBinaryKernel  cmdBuf primarySourceArray secondarySourceArray outGradientState destination =
withObjCPtr primarySourceArray $ \raw_primarySourceArray ->
  withObjCPtr secondarySourceArray $ \raw_secondarySourceArray ->
    withObjCPtr outGradientState $ \raw_outGradientState ->
      withObjCPtr destination $ \raw_destination ->
          sendMsg mpsndArrayBinaryKernel (mkSelector "encodeToCommandBuffer:primarySourceArray:secondarySourceArray:resultState:destinationArray:") retVoid [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_primarySourceArray :: Ptr ()), argPtr (castPtr raw_secondarySourceArray :: Ptr ()), argPtr (castPtr raw_outGradientState :: Ptr ()), argPtr (castPtr raw_destination :: Ptr ())]

-- | primaryEdgeMode
--
-- The edge mode used for a source NDArray             Default: MPSImageEdgeModeZero
--
-- ObjC selector: @- primaryEdgeMode@
primaryEdgeMode :: IsMPSNDArrayBinaryKernel mpsndArrayBinaryKernel => mpsndArrayBinaryKernel -> IO MPSImageEdgeMode
primaryEdgeMode mpsndArrayBinaryKernel  =
  fmap (coerce :: CULong -> MPSImageEdgeMode) $ sendMsg mpsndArrayBinaryKernel (mkSelector "primaryEdgeMode") retCULong []

-- | secondaryEdgeMode
--
-- The edge mode used for a source NDArray             Default: MPSImageEdgeModeZero
--
-- ObjC selector: @- secondaryEdgeMode@
secondaryEdgeMode :: IsMPSNDArrayBinaryKernel mpsndArrayBinaryKernel => mpsndArrayBinaryKernel -> IO MPSImageEdgeMode
secondaryEdgeMode mpsndArrayBinaryKernel  =
  fmap (coerce :: CULong -> MPSImageEdgeMode) $ sendMsg mpsndArrayBinaryKernel (mkSelector "secondaryEdgeMode") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:sourceCount:@
initWithDevice_sourceCountSelector :: Selector
initWithDevice_sourceCountSelector = mkSelector "initWithDevice:sourceCount:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:primarySourceArray:secondarySourceArray:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArraySelector :: Selector
encodeToCommandBuffer_primarySourceArray_secondarySourceArraySelector = mkSelector "encodeToCommandBuffer:primarySourceArray:secondarySourceArray:"

-- | @Selector@ for @encodeToCommandBuffer:primarySourceArray:secondarySourceArray:destinationArray:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_destinationArraySelector :: Selector
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_destinationArraySelector = mkSelector "encodeToCommandBuffer:primarySourceArray:secondarySourceArray:destinationArray:"

-- | @Selector@ for @encodeToCommandBuffer:primarySourceArray:secondarySourceArray:resultState:outputStateIsTemporary:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_resultState_outputStateIsTemporarySelector :: Selector
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_resultState_outputStateIsTemporarySelector = mkSelector "encodeToCommandBuffer:primarySourceArray:secondarySourceArray:resultState:outputStateIsTemporary:"

-- | @Selector@ for @encodeToCommandBuffer:primarySourceArray:secondarySourceArray:resultState:destinationArray:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_resultState_destinationArraySelector :: Selector
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_resultState_destinationArraySelector = mkSelector "encodeToCommandBuffer:primarySourceArray:secondarySourceArray:resultState:destinationArray:"

-- | @Selector@ for @primaryEdgeMode@
primaryEdgeModeSelector :: Selector
primaryEdgeModeSelector = mkSelector "primaryEdgeMode"

-- | @Selector@ for @secondaryEdgeMode@
secondaryEdgeModeSelector :: Selector
secondaryEdgeModeSelector = mkSelector "secondaryEdgeMode"

