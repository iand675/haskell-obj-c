{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNDArrayQuantizedMatrixMultiplication
--
-- This depends on Metal.framework.
--
-- A quantized matrix multiplication kernel: C = AB, where each input A and B can be quantized.
--
-- The kernel works with 2-8 inputs, order of inputs: First all LHS inputs, then all RHS inputs.              The order of inputs for LUT based LHS or RHS: 1) quantized input 2) Lookup Table.              The order of inputs for affine LHS or RHS: 1) quantized input 2) scale 3) zeropoint 4) minValue.              The full order of inputs for the encode methods is:                   `[LHS, RHS, <LHS quantization inputs>, <RHS quantization inputs>]`,              where @LHS@ is the left input (quantized or float) @RHS@ is the right input (quantized or float) and              `<LHS quantization inputs>` are the auxiliary quantization inputs for the LHS array (scales, zeropoints etc).              and `<RHS quantization inputs>` are the auxiliary quantization input for the RHS array.              The inputs are provided as a compacted `NSArray<MPSNDArray *>`, for example for computing              @C = A * B^T@ where @A@ is quantized with a LUT and @B@ is quantized with affine quantization that              uses scale and minValue the array of inputs is:              @ [ Aq, Bq^T, ALUT, BScale^T, BMin^T ] @.              NOTE: For affine scale, zeropoint and minValue must have same transposes as quantized input.
--
-- Generated bindings for @MPSNDArrayQuantizedMatrixMultiplication@.
module ObjC.MetalPerformanceShaders.MPSNDArrayQuantizedMatrixMultiplication
  ( MPSNDArrayQuantizedMatrixMultiplication
  , IsMPSNDArrayQuantizedMatrixMultiplication(..)
  , initWithDevice_sourceCount
  , initWithDevice_leftQuantizationDescriptor_rightQuantizationDescriptor
  , initWithDevice_leftQuantizationDescriptor_rightQuantizationDescriptorSelector
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
initWithDevice_sourceCount :: IsMPSNDArrayQuantizedMatrixMultiplication mpsndArrayQuantizedMatrixMultiplication => mpsndArrayQuantizedMatrixMultiplication -> RawId -> CULong -> IO (Id MPSNDArrayQuantizedMatrixMultiplication)
initWithDevice_sourceCount mpsndArrayQuantizedMatrixMultiplication device sourceCount =
  sendOwnedMessage mpsndArrayQuantizedMatrixMultiplication initWithDevice_sourceCountSelector device sourceCount

-- | Initializes a quantized matrix multiplication kernel.
--
-- @leftQuantizationDescriptor@ — The quantization definition for the LHS input.
--
-- @rightQuantizationDescriptor@ — The quantization definition for the RHS input.
--
-- Returns: A new valid quantized matrix multiplication kernel.
--
-- ObjC selector: @- initWithDevice:leftQuantizationDescriptor:rightQuantizationDescriptor:@
initWithDevice_leftQuantizationDescriptor_rightQuantizationDescriptor :: (IsMPSNDArrayQuantizedMatrixMultiplication mpsndArrayQuantizedMatrixMultiplication, IsMPSNDArrayQuantizationDescriptor leftQuantizationDescriptor, IsMPSNDArrayQuantizationDescriptor rightQuantizationDescriptor) => mpsndArrayQuantizedMatrixMultiplication -> RawId -> leftQuantizationDescriptor -> rightQuantizationDescriptor -> IO (Id MPSNDArrayQuantizedMatrixMultiplication)
initWithDevice_leftQuantizationDescriptor_rightQuantizationDescriptor mpsndArrayQuantizedMatrixMultiplication device leftQuantizationDescriptor rightQuantizationDescriptor =
  sendOwnedMessage mpsndArrayQuantizedMatrixMultiplication initWithDevice_leftQuantizationDescriptor_rightQuantizationDescriptorSelector device (toMPSNDArrayQuantizationDescriptor leftQuantizationDescriptor) (toMPSNDArrayQuantizationDescriptor rightQuantizationDescriptor)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:sourceCount:@
initWithDevice_sourceCountSelector :: Selector '[RawId, CULong] (Id MPSNDArrayQuantizedMatrixMultiplication)
initWithDevice_sourceCountSelector = mkSelector "initWithDevice:sourceCount:"

-- | @Selector@ for @initWithDevice:leftQuantizationDescriptor:rightQuantizationDescriptor:@
initWithDevice_leftQuantizationDescriptor_rightQuantizationDescriptorSelector :: Selector '[RawId, Id MPSNDArrayQuantizationDescriptor, Id MPSNDArrayQuantizationDescriptor] (Id MPSNDArrayQuantizedMatrixMultiplication)
initWithDevice_leftQuantizationDescriptor_rightQuantizationDescriptorSelector = mkSelector "initWithDevice:leftQuantizationDescriptor:rightQuantizationDescriptor:"

