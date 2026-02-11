{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSNDArrayMultiaryKernel@.
module ObjC.MetalPerformanceShaders.MPSNDArrayMultiaryKernel
  ( MPSNDArrayMultiaryKernel
  , IsMPSNDArrayMultiaryKernel(..)
  , initWithDevice_sourceCount
  , initWithCoder_device
  , encodeToCommandBuffer_sourceArrays
  , encodeToCommandBuffer_sourceArrays_destinationArray
  , encodeToCommandBuffer_sourceArrays_resultState_outputStateIsTemporary
  , encodeToCommandBuffer_sourceArrays_resultState_destinationArray
  , encodeToCommandEncoder_commandBuffer_sourceArrays_destinationArray
  , initWithDevice_sourceCountSelector
  , initWithCoder_deviceSelector
  , encodeToCommandBuffer_sourceArraysSelector
  , encodeToCommandBuffer_sourceArrays_destinationArraySelector
  , encodeToCommandBuffer_sourceArrays_resultState_outputStateIsTemporarySelector
  , encodeToCommandBuffer_sourceArrays_resultState_destinationArraySelector
  , encodeToCommandEncoder_commandBuffer_sourceArrays_destinationArraySelector


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
initWithDevice_sourceCount :: IsMPSNDArrayMultiaryKernel mpsndArrayMultiaryKernel => mpsndArrayMultiaryKernel -> RawId -> CULong -> IO (Id MPSNDArrayMultiaryKernel)
initWithDevice_sourceCount mpsndArrayMultiaryKernel  device count =
  sendMsg mpsndArrayMultiaryKernel (mkSelector "initWithDevice:sourceCount:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral count)] >>= ownedObject . castPtr

-- | @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNDArrayMultiaryKernel mpsndArrayMultiaryKernel, IsNSCoder coder) => mpsndArrayMultiaryKernel -> coder -> RawId -> IO (Id MPSNDArrayMultiaryKernel)
initWithCoder_device mpsndArrayMultiaryKernel  coder device =
withObjCPtr coder $ \raw_coder ->
    sendMsg mpsndArrayMultiaryKernel (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Encode a simple inference NDArray kernel and return a NDArray to hold the result
--
-- @cmdBuf@ — The command buffer into which to encode the kernel
--
-- @sourceArrays@ — The list of sources for the filter in a NSArray.                              Ordering to be defined by subclass
--
-- Returns: A newly allocated MPSNDArray that will contain the result of the calculation              when the command buffer completes successfully.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceArrays:@
encodeToCommandBuffer_sourceArrays :: (IsMPSNDArrayMultiaryKernel mpsndArrayMultiaryKernel, IsNSArray sourceArrays) => mpsndArrayMultiaryKernel -> RawId -> sourceArrays -> IO (Id MPSNDArray)
encodeToCommandBuffer_sourceArrays mpsndArrayMultiaryKernel  cmdBuf sourceArrays =
withObjCPtr sourceArrays $ \raw_sourceArrays ->
    sendMsg mpsndArrayMultiaryKernel (mkSelector "encodeToCommandBuffer:sourceArrays:") (retPtr retVoid) [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_sourceArrays :: Ptr ())] >>= retainedObject . castPtr

-- | Encode a simple inference NDArray kernel and return a NDArray to hold the result
--
-- @cmdBuf@ — The command buffer into which to encode the kernel
--
-- @sourceArrays@ — The list of sources for the filter in a NSArray.                              Ordering to be defined by subclass
--
-- @destination@ — The NDArray to receive the result
--
-- ObjC selector: @- encodeToCommandBuffer:sourceArrays:destinationArray:@
encodeToCommandBuffer_sourceArrays_destinationArray :: (IsMPSNDArrayMultiaryKernel mpsndArrayMultiaryKernel, IsNSArray sourceArrays, IsMPSNDArray destination) => mpsndArrayMultiaryKernel -> RawId -> sourceArrays -> destination -> IO ()
encodeToCommandBuffer_sourceArrays_destinationArray mpsndArrayMultiaryKernel  cmdBuf sourceArrays destination =
withObjCPtr sourceArrays $ \raw_sourceArrays ->
  withObjCPtr destination $ \raw_destination ->
      sendMsg mpsndArrayMultiaryKernel (mkSelector "encodeToCommandBuffer:sourceArrays:destinationArray:") retVoid [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_sourceArrays :: Ptr ()), argPtr (castPtr raw_destination :: Ptr ())]

-- | Encode a simple inference NDArray kernel and return a NDArray to hold the result
--
-- @cmdBuf@ — The command buffer into which to encode the kernel
--
-- @sourceArrays@ — The list of sources for the filter in a NSArray.                              Ordering to be defined by subclass
--
-- @outGradientState@ — If non-nil, the address output gradient state is written to this address
--
-- @outputStateIsTemporary@ — If YES, the state if any will be allocated to contain temporary textures and buffers as needed
--
-- Returns: A newly allocated MPSNDArray that will contain the result of the calculation              when the command buffer completes successfully.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceArrays:resultState:outputStateIsTemporary:@
encodeToCommandBuffer_sourceArrays_resultState_outputStateIsTemporary :: (IsMPSNDArrayMultiaryKernel mpsndArrayMultiaryKernel, IsNSArray sourceArrays, IsMPSState outGradientState) => mpsndArrayMultiaryKernel -> RawId -> sourceArrays -> outGradientState -> Bool -> IO (Id MPSNDArray)
encodeToCommandBuffer_sourceArrays_resultState_outputStateIsTemporary mpsndArrayMultiaryKernel  cmdBuf sourceArrays outGradientState outputStateIsTemporary =
withObjCPtr sourceArrays $ \raw_sourceArrays ->
  withObjCPtr outGradientState $ \raw_outGradientState ->
      sendMsg mpsndArrayMultiaryKernel (mkSelector "encodeToCommandBuffer:sourceArrays:resultState:outputStateIsTemporary:") (retPtr retVoid) [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_sourceArrays :: Ptr ()), argPtr (castPtr raw_outGradientState :: Ptr ()), argCULong (if outputStateIsTemporary then 1 else 0)] >>= retainedObject . castPtr

-- | Encode a simple inference NDArray kernel and return a NDArray to hold the result
--
-- @cmdBuf@ — The command buffer into which to encode the kernel
--
-- @sourceArrays@ — The list of sources for the filter in a NSArray.                              Ordering to be defined by subclass
--
-- @outGradientState@ — The output gradient state to record the operation for later use by gradient
--
-- @destination@ — A destination array to contain the result of the calculation              when the command buffer completes successfully.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceArrays:resultState:destinationArray:@
encodeToCommandBuffer_sourceArrays_resultState_destinationArray :: (IsMPSNDArrayMultiaryKernel mpsndArrayMultiaryKernel, IsNSArray sourceArrays, IsMPSState outGradientState, IsMPSNDArray destination) => mpsndArrayMultiaryKernel -> RawId -> sourceArrays -> outGradientState -> destination -> IO ()
encodeToCommandBuffer_sourceArrays_resultState_destinationArray mpsndArrayMultiaryKernel  cmdBuf sourceArrays outGradientState destination =
withObjCPtr sourceArrays $ \raw_sourceArrays ->
  withObjCPtr outGradientState $ \raw_outGradientState ->
    withObjCPtr destination $ \raw_destination ->
        sendMsg mpsndArrayMultiaryKernel (mkSelector "encodeToCommandBuffer:sourceArrays:resultState:destinationArray:") retVoid [argPtr (castPtr (unRawId cmdBuf) :: Ptr ()), argPtr (castPtr raw_sourceArrays :: Ptr ()), argPtr (castPtr raw_outGradientState :: Ptr ()), argPtr (castPtr raw_destination :: Ptr ())]

-- | Encode a simple inference NDArray kernel and return a NDArray to hold the result
--
-- @encoder@ — The MTLComputeCommandEncoder that the kernel will be encoded on
--
-- @commandBuffer@ — The command buffer into which to encode the kernel
--
-- @sourceArrays@ — The list of sources for the filter in a NSArray.                                Ordering to be defined by subclass
--
-- @destination@ — A destination array to contain the result of the calculation              when the command buffer completes successfully.
--
-- ObjC selector: @- encodeToCommandEncoder:commandBuffer:sourceArrays:destinationArray:@
encodeToCommandEncoder_commandBuffer_sourceArrays_destinationArray :: (IsMPSNDArrayMultiaryKernel mpsndArrayMultiaryKernel, IsNSArray sourceArrays, IsMPSNDArray destination) => mpsndArrayMultiaryKernel -> RawId -> RawId -> sourceArrays -> destination -> IO ()
encodeToCommandEncoder_commandBuffer_sourceArrays_destinationArray mpsndArrayMultiaryKernel  encoder commandBuffer sourceArrays destination =
withObjCPtr sourceArrays $ \raw_sourceArrays ->
  withObjCPtr destination $ \raw_destination ->
      sendMsg mpsndArrayMultiaryKernel (mkSelector "encodeToCommandEncoder:commandBuffer:sourceArrays:destinationArray:") retVoid [argPtr (castPtr (unRawId encoder) :: Ptr ()), argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceArrays :: Ptr ()), argPtr (castPtr raw_destination :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:sourceCount:@
initWithDevice_sourceCountSelector :: Selector
initWithDevice_sourceCountSelector = mkSelector "initWithDevice:sourceCount:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArrays:@
encodeToCommandBuffer_sourceArraysSelector :: Selector
encodeToCommandBuffer_sourceArraysSelector = mkSelector "encodeToCommandBuffer:sourceArrays:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArrays:destinationArray:@
encodeToCommandBuffer_sourceArrays_destinationArraySelector :: Selector
encodeToCommandBuffer_sourceArrays_destinationArraySelector = mkSelector "encodeToCommandBuffer:sourceArrays:destinationArray:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArrays:resultState:outputStateIsTemporary:@
encodeToCommandBuffer_sourceArrays_resultState_outputStateIsTemporarySelector :: Selector
encodeToCommandBuffer_sourceArrays_resultState_outputStateIsTemporarySelector = mkSelector "encodeToCommandBuffer:sourceArrays:resultState:outputStateIsTemporary:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArrays:resultState:destinationArray:@
encodeToCommandBuffer_sourceArrays_resultState_destinationArraySelector :: Selector
encodeToCommandBuffer_sourceArrays_resultState_destinationArraySelector = mkSelector "encodeToCommandBuffer:sourceArrays:resultState:destinationArray:"

-- | @Selector@ for @encodeToCommandEncoder:commandBuffer:sourceArrays:destinationArray:@
encodeToCommandEncoder_commandBuffer_sourceArrays_destinationArraySelector :: Selector
encodeToCommandEncoder_commandBuffer_sourceArrays_destinationArraySelector = mkSelector "encodeToCommandEncoder:commandBuffer:sourceArrays:destinationArray:"

