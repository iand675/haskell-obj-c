{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNDArrayIdentityKernel
--
-- This depends on Metal.framework.
--
-- An efficient kernel to handle copies, transposed-copies and reshapes.
--
-- Generated bindings for @MPSNDArrayIdentity@.
module ObjC.MetalPerformanceShaders.MPSNDArrayIdentity
  ( MPSNDArrayIdentity
  , IsMPSNDArrayIdentity(..)
  , initWithDevice
  , reshapeWithCommandBuffer_sourceArray_shape_destinationArray
  , reshapeWithCommandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArray
  , reshapeWithCommandEncoder_commandBuffer_sourceArray_shape_destinationArray
  , reshapeWithCommandEncoder_commandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArray
  , initWithDeviceSelector
  , reshapeWithCommandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArraySelector
  , reshapeWithCommandBuffer_sourceArray_shape_destinationArraySelector
  , reshapeWithCommandEncoder_commandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArraySelector
  , reshapeWithCommandEncoder_commandBuffer_sourceArray_shape_destinationArraySelector


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
initWithDevice :: IsMPSNDArrayIdentity mpsndArrayIdentity => mpsndArrayIdentity -> RawId -> IO (Id MPSNDArrayIdentity)
initWithDevice mpsndArrayIdentity device =
  sendOwnedMessage mpsndArrayIdentity initWithDeviceSelector device

-- | Do a reshape operation, either by trying to alias the array, returning an arrayview, or by copying.
--
-- @cmdBuf@ — The command buffer into which to encode the kernel, or to create a temporary array alias.
--
-- @sourceArray@ — Source array. If this function returns a non-nil result, then the readCount of @sourceArray@ is decremented.
--
-- @shape@ — The new shape, given in TF dimension ordering (as always with MPSShape).
--
-- @destinationArray@ — If not nil, then the result of reshape will be copied to this. Shape of @destinationArray@ must match @shape@.
--
-- Returns: If @destinationArray@ is not nil, then @destinationArray@. Otherwise aliasing is tried, and if aliasing is not possible              due to existing slices or transposes nil is returned. If aliasing is successful, then a new arrayview of @sourceArray@              is returned; If @sourceArray@ is a @MPSTemporaryArray@ then a @MPSTemporaryArray@ is returned referencing the same data,              otherwise a @MPSNDArray@ type result is returned.
--
-- ObjC selector: @- reshapeWithCommandBuffer:sourceArray:shape:destinationArray:@
reshapeWithCommandBuffer_sourceArray_shape_destinationArray :: (IsMPSNDArrayIdentity mpsndArrayIdentity, IsMPSNDArray sourceArray, IsMPSNDArray destinationArray) => mpsndArrayIdentity -> RawId -> sourceArray -> RawId -> destinationArray -> IO (Id MPSNDArray)
reshapeWithCommandBuffer_sourceArray_shape_destinationArray mpsndArrayIdentity cmdBuf sourceArray shape destinationArray =
  sendMessage mpsndArrayIdentity reshapeWithCommandBuffer_sourceArray_shape_destinationArraySelector cmdBuf (toMPSNDArray sourceArray) shape (toMPSNDArray destinationArray)

-- | @- reshapeWithCommandBuffer:sourceArray:dimensionCount:dimensionSizes:destinationArray:@
reshapeWithCommandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArray :: (IsMPSNDArrayIdentity mpsndArrayIdentity, IsMPSNDArray sourceArray, IsMPSNDArray destinationArray) => mpsndArrayIdentity -> RawId -> sourceArray -> CULong -> Ptr CULong -> destinationArray -> IO (Id MPSNDArray)
reshapeWithCommandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArray mpsndArrayIdentity cmdBuf sourceArray numberOfDimensions dimensionSizes destinationArray =
  sendMessage mpsndArrayIdentity reshapeWithCommandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArraySelector cmdBuf (toMPSNDArray sourceArray) numberOfDimensions dimensionSizes (toMPSNDArray destinationArray)

-- | @- reshapeWithCommandEncoder:commandBuffer:sourceArray:shape:destinationArray:@
reshapeWithCommandEncoder_commandBuffer_sourceArray_shape_destinationArray :: (IsMPSNDArrayIdentity mpsndArrayIdentity, IsMPSNDArray sourceArray, IsMPSNDArray destinationArray) => mpsndArrayIdentity -> RawId -> RawId -> sourceArray -> RawId -> destinationArray -> IO (Id MPSNDArray)
reshapeWithCommandEncoder_commandBuffer_sourceArray_shape_destinationArray mpsndArrayIdentity encoder cmdBuf sourceArray shape destinationArray =
  sendMessage mpsndArrayIdentity reshapeWithCommandEncoder_commandBuffer_sourceArray_shape_destinationArraySelector encoder cmdBuf (toMPSNDArray sourceArray) shape (toMPSNDArray destinationArray)

-- | @- reshapeWithCommandEncoder:commandBuffer:sourceArray:dimensionCount:dimensionSizes:destinationArray:@
reshapeWithCommandEncoder_commandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArray :: (IsMPSNDArrayIdentity mpsndArrayIdentity, IsMPSNDArray sourceArray, IsMPSNDArray destinationArray) => mpsndArrayIdentity -> RawId -> RawId -> sourceArray -> CULong -> Ptr CULong -> destinationArray -> IO (Id MPSNDArray)
reshapeWithCommandEncoder_commandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArray mpsndArrayIdentity encoder cmdBuf sourceArray numberOfDimensions dimensionSizes destinationArray =
  sendMessage mpsndArrayIdentity reshapeWithCommandEncoder_commandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArraySelector encoder cmdBuf (toMPSNDArray sourceArray) numberOfDimensions dimensionSizes (toMPSNDArray destinationArray)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNDArrayIdentity)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @reshapeWithCommandBuffer:sourceArray:shape:destinationArray:@
reshapeWithCommandBuffer_sourceArray_shape_destinationArraySelector :: Selector '[RawId, Id MPSNDArray, RawId, Id MPSNDArray] (Id MPSNDArray)
reshapeWithCommandBuffer_sourceArray_shape_destinationArraySelector = mkSelector "reshapeWithCommandBuffer:sourceArray:shape:destinationArray:"

-- | @Selector@ for @reshapeWithCommandBuffer:sourceArray:dimensionCount:dimensionSizes:destinationArray:@
reshapeWithCommandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArraySelector :: Selector '[RawId, Id MPSNDArray, CULong, Ptr CULong, Id MPSNDArray] (Id MPSNDArray)
reshapeWithCommandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArraySelector = mkSelector "reshapeWithCommandBuffer:sourceArray:dimensionCount:dimensionSizes:destinationArray:"

-- | @Selector@ for @reshapeWithCommandEncoder:commandBuffer:sourceArray:shape:destinationArray:@
reshapeWithCommandEncoder_commandBuffer_sourceArray_shape_destinationArraySelector :: Selector '[RawId, RawId, Id MPSNDArray, RawId, Id MPSNDArray] (Id MPSNDArray)
reshapeWithCommandEncoder_commandBuffer_sourceArray_shape_destinationArraySelector = mkSelector "reshapeWithCommandEncoder:commandBuffer:sourceArray:shape:destinationArray:"

-- | @Selector@ for @reshapeWithCommandEncoder:commandBuffer:sourceArray:dimensionCount:dimensionSizes:destinationArray:@
reshapeWithCommandEncoder_commandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArraySelector :: Selector '[RawId, RawId, Id MPSNDArray, CULong, Ptr CULong, Id MPSNDArray] (Id MPSNDArray)
reshapeWithCommandEncoder_commandBuffer_sourceArray_dimensionCount_dimensionSizes_destinationArraySelector = mkSelector "reshapeWithCommandEncoder:commandBuffer:sourceArray:dimensionCount:dimensionSizes:destinationArray:"

