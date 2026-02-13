{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSNDArrayUnaryKernel@.
module ObjC.MetalPerformanceShaders.MPSNDArrayUnaryKernel
  ( MPSNDArrayUnaryKernel
  , IsMPSNDArrayUnaryKernel(..)
  , initWithDevice
  , initWithDevice_sourceCount
  , initWithCoder_device
  , encodeToCommandBuffer_sourceArray
  , encodeToCommandBuffer_sourceArray_destinationArray
  , encodeToCommandBuffer_sourceArray_resultState_outputStateIsTemporary
  , encodeToCommandBuffer_sourceArray_resultState_destinationArray
  , edgeMode
  , edgeModeSelector
  , encodeToCommandBuffer_sourceArraySelector
  , encodeToCommandBuffer_sourceArray_destinationArraySelector
  , encodeToCommandBuffer_sourceArray_resultState_destinationArraySelector
  , encodeToCommandBuffer_sourceArray_resultState_outputStateIsTemporarySelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_sourceCountSelector

  -- * Enum types
  , MPSImageEdgeMode(MPSImageEdgeMode)
  , pattern MPSImageEdgeModeZero
  , pattern MPSImageEdgeModeClamp
  , pattern MPSImageEdgeModeMirror
  , pattern MPSImageEdgeModeMirrorWithEdge
  , pattern MPSImageEdgeModeConstant

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:@
initWithDevice :: IsMPSNDArrayUnaryKernel mpsndArrayUnaryKernel => mpsndArrayUnaryKernel -> RawId -> IO (Id MPSNDArrayUnaryKernel)
initWithDevice mpsndArrayUnaryKernel device =
  sendOwnedMessage mpsndArrayUnaryKernel initWithDeviceSelector device

-- | @- initWithDevice:sourceCount:@
initWithDevice_sourceCount :: IsMPSNDArrayUnaryKernel mpsndArrayUnaryKernel => mpsndArrayUnaryKernel -> RawId -> CULong -> IO (Id MPSNDArrayUnaryKernel)
initWithDevice_sourceCount mpsndArrayUnaryKernel device count =
  sendOwnedMessage mpsndArrayUnaryKernel initWithDevice_sourceCountSelector device count

-- | @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNDArrayUnaryKernel mpsndArrayUnaryKernel, IsNSCoder coder) => mpsndArrayUnaryKernel -> coder -> RawId -> IO (Id MPSNDArrayUnaryKernel)
initWithCoder_device mpsndArrayUnaryKernel coder device =
  sendOwnedMessage mpsndArrayUnaryKernel initWithCoder_deviceSelector (toNSCoder coder) device

-- | Encode a simple inference NDArray kernel and return a NDArray to hold the result
--
-- @cmdBuf@ — The command buffer into which to encode the kernel
--
-- @sourceArray@ — The source for the filter in an NSArray.
--
-- Returns: A newly allocated MPSNDArray that will contain the result of the calculation              when the command buffer completes successfully.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceArray:@
encodeToCommandBuffer_sourceArray :: (IsMPSNDArrayUnaryKernel mpsndArrayUnaryKernel, IsMPSNDArray sourceArray) => mpsndArrayUnaryKernel -> RawId -> sourceArray -> IO (Id MPSNDArray)
encodeToCommandBuffer_sourceArray mpsndArrayUnaryKernel cmdBuf sourceArray =
  sendMessage mpsndArrayUnaryKernel encodeToCommandBuffer_sourceArraySelector cmdBuf (toMPSNDArray sourceArray)

-- | Encode a simple inference NDArray kernel and return a NDArray to hold the result
--
-- @cmdBuf@ — The command buffer into which to encode the kernel
--
-- @sourceArray@ — The source for the filter in an NSArray.
--
-- @destination@ — The NDArray to receive the result
--
-- ObjC selector: @- encodeToCommandBuffer:sourceArray:destinationArray:@
encodeToCommandBuffer_sourceArray_destinationArray :: (IsMPSNDArrayUnaryKernel mpsndArrayUnaryKernel, IsMPSNDArray sourceArray, IsMPSNDArray destination) => mpsndArrayUnaryKernel -> RawId -> sourceArray -> destination -> IO ()
encodeToCommandBuffer_sourceArray_destinationArray mpsndArrayUnaryKernel cmdBuf sourceArray destination =
  sendMessage mpsndArrayUnaryKernel encodeToCommandBuffer_sourceArray_destinationArraySelector cmdBuf (toMPSNDArray sourceArray) (toMPSNDArray destination)

-- | Encode a simple inference NDArray kernel and return a NDArray to hold the result
--
-- @cmdBuf@ — The command buffer into which to encode the kernel
--
-- @sourceArray@ — The source for the filter in an NSArray.
--
-- @outGradientState@ — If non-nil, the address output gradient state is written to this address
--
-- @outputStateIsTemporary@ — If YES, the state if any will be allocated to contain temporary textures and buffers as needed
--
-- Returns: A newly allocated MPSNDArray that will contain the result of the calculation              when the command buffer completes successfully.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceArray:resultState:outputStateIsTemporary:@
encodeToCommandBuffer_sourceArray_resultState_outputStateIsTemporary :: (IsMPSNDArrayUnaryKernel mpsndArrayUnaryKernel, IsMPSNDArray sourceArray, IsMPSState outGradientState) => mpsndArrayUnaryKernel -> RawId -> sourceArray -> outGradientState -> Bool -> IO (Id MPSNDArray)
encodeToCommandBuffer_sourceArray_resultState_outputStateIsTemporary mpsndArrayUnaryKernel cmdBuf sourceArray outGradientState outputStateIsTemporary =
  sendMessage mpsndArrayUnaryKernel encodeToCommandBuffer_sourceArray_resultState_outputStateIsTemporarySelector cmdBuf (toMPSNDArray sourceArray) (toMPSState outGradientState) outputStateIsTemporary

-- | Encode a simple inference NDArray kernel and return a NDArray to hold the result
--
-- @cmdBuf@ — The command buffer into which to encode the kernel
--
-- @sourceArray@ — The source for the filter in an NSArray.
--
-- @outGradientState@ — The output gradient state to record the operation for later use by gradient
--
-- @destination@ — A destination array to contain the result of the calculation              when the command buffer completes successfully.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceArray:resultState:destinationArray:@
encodeToCommandBuffer_sourceArray_resultState_destinationArray :: (IsMPSNDArrayUnaryKernel mpsndArrayUnaryKernel, IsMPSNDArray sourceArray, IsMPSState outGradientState, IsMPSNDArray destination) => mpsndArrayUnaryKernel -> RawId -> sourceArray -> outGradientState -> destination -> IO ()
encodeToCommandBuffer_sourceArray_resultState_destinationArray mpsndArrayUnaryKernel cmdBuf sourceArray outGradientState destination =
  sendMessage mpsndArrayUnaryKernel encodeToCommandBuffer_sourceArray_resultState_destinationArraySelector cmdBuf (toMPSNDArray sourceArray) (toMPSState outGradientState) (toMPSNDArray destination)

-- | edgeMode
--
-- The edge mode used for a source NDArray             Default: MPSImageEdgeModeZero
--
-- ObjC selector: @- edgeMode@
edgeMode :: IsMPSNDArrayUnaryKernel mpsndArrayUnaryKernel => mpsndArrayUnaryKernel -> IO MPSImageEdgeMode
edgeMode mpsndArrayUnaryKernel =
  sendMessage mpsndArrayUnaryKernel edgeModeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNDArrayUnaryKernel)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:sourceCount:@
initWithDevice_sourceCountSelector :: Selector '[RawId, CULong] (Id MPSNDArrayUnaryKernel)
initWithDevice_sourceCountSelector = mkSelector "initWithDevice:sourceCount:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNDArrayUnaryKernel)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArray:@
encodeToCommandBuffer_sourceArraySelector :: Selector '[RawId, Id MPSNDArray] (Id MPSNDArray)
encodeToCommandBuffer_sourceArraySelector = mkSelector "encodeToCommandBuffer:sourceArray:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArray:destinationArray:@
encodeToCommandBuffer_sourceArray_destinationArraySelector :: Selector '[RawId, Id MPSNDArray, Id MPSNDArray] ()
encodeToCommandBuffer_sourceArray_destinationArraySelector = mkSelector "encodeToCommandBuffer:sourceArray:destinationArray:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArray:resultState:outputStateIsTemporary:@
encodeToCommandBuffer_sourceArray_resultState_outputStateIsTemporarySelector :: Selector '[RawId, Id MPSNDArray, Id MPSState, Bool] (Id MPSNDArray)
encodeToCommandBuffer_sourceArray_resultState_outputStateIsTemporarySelector = mkSelector "encodeToCommandBuffer:sourceArray:resultState:outputStateIsTemporary:"

-- | @Selector@ for @encodeToCommandBuffer:sourceArray:resultState:destinationArray:@
encodeToCommandBuffer_sourceArray_resultState_destinationArraySelector :: Selector '[RawId, Id MPSNDArray, Id MPSState, Id MPSNDArray] ()
encodeToCommandBuffer_sourceArray_resultState_destinationArraySelector = mkSelector "encodeToCommandBuffer:sourceArray:resultState:destinationArray:"

-- | @Selector@ for @edgeMode@
edgeModeSelector :: Selector '[] MPSImageEdgeMode
edgeModeSelector = mkSelector "edgeMode"

