{-# LANGUAGE DataKinds #-}
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
  , encodeToCommandBuffer_sourceArraysSelector
  , encodeToCommandBuffer_sourceArrays_destinationArraySelector
  , encodeToCommandBuffer_sourceArrays_resultState_destinationArraySelector
  , encodeToCommandBuffer_sourceArrays_resultState_outputStateIsTemporarySelector
  , encodeToCommandEncoder_commandBuffer_sourceArrays_destinationArraySelector
  , initWithCoder_deviceSelector
  , initWithDevice_sourceCountSelector


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
initWithDevice_sourceCount :: IsMPSNDArrayMultiaryKernel mpsndArrayMultiaryKernel => mpsndArrayMultiaryKernel -> RawId -> CULong -> IO (Id MPSNDArrayMultiaryKernel)
initWithDevice_sourceCount mpsndArrayMultiaryKernel device count =
  sendOwnedMessage mpsndArrayMultiaryKernel initWithDevice_sourceCountSelector device count

-- | @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNDArrayMultiaryKernel mpsndArrayMultiaryKernel, IsNSCoder coder) => mpsndArrayMultiaryKernel -> coder -> RawId -> IO (Id MPSNDArrayMultiaryKernel)
initWithCoder_device mpsndArrayMultiaryKernel coder device =
  sendOwnedMessage mpsndArrayMultiaryKernel initWithCoder_deviceSelector (toNSCoder coder) device

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
encodeToCommandBuffer_sourceArrays mpsndArrayMultiaryKernel cmdBuf sourceArrays =
  sendMessage mpsndArrayMultiaryKernel encodeToCommandBuffer_sourceArraysSelector cmdBuf (toNSArray sourceArrays)

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
encodeToCommandBuffer_sourceArrays_destinationArray mpsndArrayMultiaryKernel cmdBuf sourceArrays destination =
  sendMessage mpsndArrayMultiaryKernel encodeToCommandBuffer_sourceArrays_destinationArraySelector cmdBuf (toNSArray sourceArrays) (toMPSNDArray destination)

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
encodeToCommandBuffer_sourceArrays_resultState_outputStateIsTemporary mpsndArrayMultiaryKernel cmdBuf sourceArrays outGradientState outputStateIsTemporary =
  sendMessage mpsndArrayMultiaryKernel encodeToCommandBuffer_sourceArrays_resultState_outputStateIsTemporarySelector cmdBuf (toNSArray sourceArrays) (toMPSState outGradientState) outputStateIsTemporary

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
encodeToCommandBuffer_sourceArrays_resultState_destinationArray mpsndArrayMultiaryKernel cmdBuf sourceArrays outGradientState destination =
  sendMessage mpsndArrayMultiaryKernel encodeToCommandBuffer_sourceArrays_resultState_destinationArraySelector cmdBuf (toNSArray sourceArrays) (toMPSState outGradientState) (toMPSNDArray destination)

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
encodeToCommandEncoder_commandBuffer_sourceArrays_destinationArray mpsndArrayMultiaryKernel encoder commandBuffer sourceArrays destination =
  sendMessage mpsndArrayMultiaryKernel encodeToCommandEncoder_commandBuffer_sourceArrays_destinationArraySelector encoder commandBuffer (toNSArray sourceArrays) (toMPSNDArray destination)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:sourceCount:@
initWithDevice_sourceCountSelector :: Selector '[RawId, CULong] (Id MPSNDArrayMultiaryKernel)
initWithDevice_sourceCountSelector = mkSelector "initWithDevice:sourceCount:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNDArrayMultiaryKernel)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArrays:@
encodeToCommandBuffer_sourceArraysSelector :: Selector '[RawId, Id NSArray] (Id MPSNDArray)
encodeToCommandBuffer_sourceArraysSelector = mkSelector "encodeToCommandBuffer:sourceArrays:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArrays:destinationArray:@
encodeToCommandBuffer_sourceArrays_destinationArraySelector :: Selector '[RawId, Id NSArray, Id MPSNDArray] ()
encodeToCommandBuffer_sourceArrays_destinationArraySelector = mkSelector "encodeToCommandBuffer:sourceArrays:destinationArray:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArrays:resultState:outputStateIsTemporary:@
encodeToCommandBuffer_sourceArrays_resultState_outputStateIsTemporarySelector :: Selector '[RawId, Id NSArray, Id MPSState, Bool] (Id MPSNDArray)
encodeToCommandBuffer_sourceArrays_resultState_outputStateIsTemporarySelector = mkSelector "encodeToCommandBuffer:sourceArrays:resultState:outputStateIsTemporary:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArrays:resultState:destinationArray:@
encodeToCommandBuffer_sourceArrays_resultState_destinationArraySelector :: Selector '[RawId, Id NSArray, Id MPSState, Id MPSNDArray] ()
encodeToCommandBuffer_sourceArrays_resultState_destinationArraySelector = mkSelector "encodeToCommandBuffer:sourceArrays:resultState:destinationArray:"

-- | @Selector@ for @encodeToCommandEncoder:commandBuffer:sourceArrays:destinationArray:@
encodeToCommandEncoder_commandBuffer_sourceArrays_destinationArraySelector :: Selector '[RawId, RawId, Id NSArray, Id MPSNDArray] ()
encodeToCommandEncoder_commandBuffer_sourceArrays_destinationArraySelector = mkSelector "encodeToCommandEncoder:commandBuffer:sourceArrays:destinationArray:"

