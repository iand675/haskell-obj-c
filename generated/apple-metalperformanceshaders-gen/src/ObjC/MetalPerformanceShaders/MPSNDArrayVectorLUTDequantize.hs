{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNDArrayVectorLUTDequantize
--
-- This depends on Metal.framework.
--
-- A kernel which dequantizes a lookup-table based NDArray with vector LUT support.
--
-- The kernel works with 2 inputs: 1) The quantized input, 2) The LookUp table array.
--
-- Generated bindings for @MPSNDArrayVectorLUTDequantize@.
module ObjC.MetalPerformanceShaders.MPSNDArrayVectorLUTDequantize
  ( MPSNDArrayVectorLUTDequantize
  , IsMPSNDArrayVectorLUTDequantize(..)
  , initWithDevice_axis
  , initWithDevice_sourceCount
  , vectorAxis
  , setVectorAxis
  , initWithDevice_axisSelector
  , initWithDevice_sourceCountSelector
  , setVectorAxisSelector
  , vectorAxisSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a kernel for vector-based LUT dequantization.
--
-- @device@ — The Metal device to be used with this kernel.
--
-- @axis@ — The vector axis in the output.
--
-- Returns: A new vector LUT dequantization kernel.
--
-- ObjC selector: @- initWithDevice:axis:@
initWithDevice_axis :: IsMPSNDArrayVectorLUTDequantize mpsndArrayVectorLUTDequantize => mpsndArrayVectorLUTDequantize -> RawId -> CULong -> IO (Id MPSNDArrayVectorLUTDequantize)
initWithDevice_axis mpsndArrayVectorLUTDequantize device axis =
  sendOwnedMessage mpsndArrayVectorLUTDequantize initWithDevice_axisSelector device axis

-- | @- initWithDevice:sourceCount:@
initWithDevice_sourceCount :: IsMPSNDArrayVectorLUTDequantize mpsndArrayVectorLUTDequantize => mpsndArrayVectorLUTDequantize -> RawId -> CULong -> IO (Id MPSNDArrayVectorLUTDequantize)
initWithDevice_sourceCount mpsndArrayVectorLUTDequantize device sourceCount =
  sendOwnedMessage mpsndArrayVectorLUTDequantize initWithDevice_sourceCountSelector device sourceCount

-- | vectorAxis
--
-- Which axis in the destination will receive the vector component, must be less than 4.
--
-- ObjC selector: @- vectorAxis@
vectorAxis :: IsMPSNDArrayVectorLUTDequantize mpsndArrayVectorLUTDequantize => mpsndArrayVectorLUTDequantize -> IO CULong
vectorAxis mpsndArrayVectorLUTDequantize =
  sendMessage mpsndArrayVectorLUTDequantize vectorAxisSelector

-- | vectorAxis
--
-- Which axis in the destination will receive the vector component, must be less than 4.
--
-- ObjC selector: @- setVectorAxis:@
setVectorAxis :: IsMPSNDArrayVectorLUTDequantize mpsndArrayVectorLUTDequantize => mpsndArrayVectorLUTDequantize -> CULong -> IO ()
setVectorAxis mpsndArrayVectorLUTDequantize value =
  sendMessage mpsndArrayVectorLUTDequantize setVectorAxisSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:axis:@
initWithDevice_axisSelector :: Selector '[RawId, CULong] (Id MPSNDArrayVectorLUTDequantize)
initWithDevice_axisSelector = mkSelector "initWithDevice:axis:"

-- | @Selector@ for @initWithDevice:sourceCount:@
initWithDevice_sourceCountSelector :: Selector '[RawId, CULong] (Id MPSNDArrayVectorLUTDequantize)
initWithDevice_sourceCountSelector = mkSelector "initWithDevice:sourceCount:"

-- | @Selector@ for @vectorAxis@
vectorAxisSelector :: Selector '[] CULong
vectorAxisSelector = mkSelector "vectorAxis"

-- | @Selector@ for @setVectorAxis:@
setVectorAxisSelector :: Selector '[CULong] ()
setVectorAxisSelector = mkSelector "setVectorAxis:"

