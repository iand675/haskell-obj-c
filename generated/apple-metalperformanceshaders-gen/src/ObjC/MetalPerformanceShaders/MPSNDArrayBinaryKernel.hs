{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , encodeToCommandBuffer_primarySourceArray_secondarySourceArraySelector
  , encodeToCommandBuffer_primarySourceArray_secondarySourceArray_destinationArraySelector
  , encodeToCommandBuffer_primarySourceArray_secondarySourceArray_resultState_destinationArraySelector
  , encodeToCommandBuffer_primarySourceArray_secondarySourceArray_resultState_outputStateIsTemporarySelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_sourceCountSelector
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
initWithDevice :: IsMPSNDArrayBinaryKernel mpsndArrayBinaryKernel => mpsndArrayBinaryKernel -> RawId -> IO (Id MPSNDArrayBinaryKernel)
initWithDevice mpsndArrayBinaryKernel device =
  sendOwnedMessage mpsndArrayBinaryKernel initWithDeviceSelector device

-- | @- initWithDevice:sourceCount:@
initWithDevice_sourceCount :: IsMPSNDArrayBinaryKernel mpsndArrayBinaryKernel => mpsndArrayBinaryKernel -> RawId -> CULong -> IO (Id MPSNDArrayBinaryKernel)
initWithDevice_sourceCount mpsndArrayBinaryKernel device count =
  sendOwnedMessage mpsndArrayBinaryKernel initWithDevice_sourceCountSelector device count

-- | @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNDArrayBinaryKernel mpsndArrayBinaryKernel, IsNSCoder coder) => mpsndArrayBinaryKernel -> coder -> RawId -> IO (Id MPSNDArrayBinaryKernel)
initWithCoder_device mpsndArrayBinaryKernel coder device =
  sendOwnedMessage mpsndArrayBinaryKernel initWithCoder_deviceSelector (toNSCoder coder) device

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
encodeToCommandBuffer_primarySourceArray_secondarySourceArray mpsndArrayBinaryKernel cmdBuf primarySourceArray secondarySourceArray =
  sendMessage mpsndArrayBinaryKernel encodeToCommandBuffer_primarySourceArray_secondarySourceArraySelector cmdBuf (toMPSNDArray primarySourceArray) (toMPSNDArray secondarySourceArray)

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
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_destinationArray mpsndArrayBinaryKernel cmdBuf primarySourceArray secondarySourceArray destination =
  sendMessage mpsndArrayBinaryKernel encodeToCommandBuffer_primarySourceArray_secondarySourceArray_destinationArraySelector cmdBuf (toMPSNDArray primarySourceArray) (toMPSNDArray secondarySourceArray) (toMPSNDArray destination)

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
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_resultState_outputStateIsTemporary mpsndArrayBinaryKernel cmdBuf primarySourceArray secondarySourceArray outGradientState outputStateIsTemporary =
  sendMessage mpsndArrayBinaryKernel encodeToCommandBuffer_primarySourceArray_secondarySourceArray_resultState_outputStateIsTemporarySelector cmdBuf (toMPSNDArray primarySourceArray) (toMPSNDArray secondarySourceArray) (toMPSState outGradientState) outputStateIsTemporary

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
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_resultState_destinationArray mpsndArrayBinaryKernel cmdBuf primarySourceArray secondarySourceArray outGradientState destination =
  sendMessage mpsndArrayBinaryKernel encodeToCommandBuffer_primarySourceArray_secondarySourceArray_resultState_destinationArraySelector cmdBuf (toMPSNDArray primarySourceArray) (toMPSNDArray secondarySourceArray) (toMPSState outGradientState) (toMPSNDArray destination)

-- | primaryEdgeMode
--
-- The edge mode used for a source NDArray             Default: MPSImageEdgeModeZero
--
-- ObjC selector: @- primaryEdgeMode@
primaryEdgeMode :: IsMPSNDArrayBinaryKernel mpsndArrayBinaryKernel => mpsndArrayBinaryKernel -> IO MPSImageEdgeMode
primaryEdgeMode mpsndArrayBinaryKernel =
  sendMessage mpsndArrayBinaryKernel primaryEdgeModeSelector

-- | secondaryEdgeMode
--
-- The edge mode used for a source NDArray             Default: MPSImageEdgeModeZero
--
-- ObjC selector: @- secondaryEdgeMode@
secondaryEdgeMode :: IsMPSNDArrayBinaryKernel mpsndArrayBinaryKernel => mpsndArrayBinaryKernel -> IO MPSImageEdgeMode
secondaryEdgeMode mpsndArrayBinaryKernel =
  sendMessage mpsndArrayBinaryKernel secondaryEdgeModeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNDArrayBinaryKernel)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:sourceCount:@
initWithDevice_sourceCountSelector :: Selector '[RawId, CULong] (Id MPSNDArrayBinaryKernel)
initWithDevice_sourceCountSelector = mkSelector "initWithDevice:sourceCount:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNDArrayBinaryKernel)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:primarySourceArray:secondarySourceArray:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArraySelector :: Selector '[RawId, Id MPSNDArray, Id MPSNDArray] (Id MPSNDArray)
encodeToCommandBuffer_primarySourceArray_secondarySourceArraySelector = mkSelector "encodeToCommandBuffer:primarySourceArray:secondarySourceArray:"

-- | @Selector@ for @encodeToCommandBuffer:primarySourceArray:secondarySourceArray:destinationArray:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_destinationArraySelector :: Selector '[RawId, Id MPSNDArray, Id MPSNDArray, Id MPSNDArray] ()
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_destinationArraySelector = mkSelector "encodeToCommandBuffer:primarySourceArray:secondarySourceArray:destinationArray:"

-- | @Selector@ for @encodeToCommandBuffer:primarySourceArray:secondarySourceArray:resultState:outputStateIsTemporary:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_resultState_outputStateIsTemporarySelector :: Selector '[RawId, Id MPSNDArray, Id MPSNDArray, Id MPSState, Bool] (Id MPSNDArray)
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_resultState_outputStateIsTemporarySelector = mkSelector "encodeToCommandBuffer:primarySourceArray:secondarySourceArray:resultState:outputStateIsTemporary:"

-- | @Selector@ for @encodeToCommandBuffer:primarySourceArray:secondarySourceArray:resultState:destinationArray:@
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_resultState_destinationArraySelector :: Selector '[RawId, Id MPSNDArray, Id MPSNDArray, Id MPSState, Id MPSNDArray] ()
encodeToCommandBuffer_primarySourceArray_secondarySourceArray_resultState_destinationArraySelector = mkSelector "encodeToCommandBuffer:primarySourceArray:secondarySourceArray:resultState:destinationArray:"

-- | @Selector@ for @primaryEdgeMode@
primaryEdgeModeSelector :: Selector '[] MPSImageEdgeMode
primaryEdgeModeSelector = mkSelector "primaryEdgeMode"

-- | @Selector@ for @secondaryEdgeMode@
secondaryEdgeModeSelector :: Selector '[] MPSImageEdgeMode
secondaryEdgeModeSelector = mkSelector "secondaryEdgeMode"

