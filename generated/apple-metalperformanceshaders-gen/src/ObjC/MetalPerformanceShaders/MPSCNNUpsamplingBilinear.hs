{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNUpsamplingBilinear
--
-- This depends on Metal.framework.
--
-- Specifies the bilinear spatial upsampling filter.
--
-- Generated bindings for @MPSCNNUpsamplingBilinear@.
module ObjC.MetalPerformanceShaders.MPSCNNUpsamplingBilinear
  ( MPSCNNUpsamplingBilinear
  , IsMPSCNNUpsamplingBilinear(..)
  , initWithDevice_integerScaleFactorX_integerScaleFactorY
  , initWithDevice_integerScaleFactorX_integerScaleFactorY_alignCorners
  , initWithDevice_integerScaleFactorX_integerScaleFactorYSelector
  , initWithDevice_integerScaleFactorX_integerScaleFactorY_alignCornersSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize the bilinear spatial upsampling filter.
--
-- @device@ — The device the filter will run on.
--
-- @integerScaleFactorX@ — The upsampling factor for the x dimension.
--
-- @integerScaleFactorY@ — The upsampling factor for the y dimension.
--
-- Returns: A valid MPSCNNUpsamplingBilinear object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:integerScaleFactorX:integerScaleFactorY:@
initWithDevice_integerScaleFactorX_integerScaleFactorY :: IsMPSCNNUpsamplingBilinear mpscnnUpsamplingBilinear => mpscnnUpsamplingBilinear -> RawId -> CULong -> CULong -> IO (Id MPSCNNUpsamplingBilinear)
initWithDevice_integerScaleFactorX_integerScaleFactorY mpscnnUpsamplingBilinear device integerScaleFactorX integerScaleFactorY =
  sendOwnedMessage mpscnnUpsamplingBilinear initWithDevice_integerScaleFactorX_integerScaleFactorYSelector device integerScaleFactorX integerScaleFactorY

-- | Initialize the bilinear spatial upsampling filter.
--
-- @device@ — The device the filter will run on.
--
-- @integerScaleFactorX@ — The upsampling factor for the x dimension.
--
-- @integerScaleFactorY@ — The upsampling factor for the y dimension.
--
-- @alignCorners@ — Specifier whether the centers of the 4 corner pixels of the input and output regions are aligned,                                      preserving the values at the corner pixels.
--
-- Returns: A valid MPSCNNUpsamplingBilinear object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:integerScaleFactorX:integerScaleFactorY:alignCorners:@
initWithDevice_integerScaleFactorX_integerScaleFactorY_alignCorners :: IsMPSCNNUpsamplingBilinear mpscnnUpsamplingBilinear => mpscnnUpsamplingBilinear -> RawId -> CULong -> CULong -> Bool -> IO (Id MPSCNNUpsamplingBilinear)
initWithDevice_integerScaleFactorX_integerScaleFactorY_alignCorners mpscnnUpsamplingBilinear device integerScaleFactorX integerScaleFactorY alignCorners =
  sendOwnedMessage mpscnnUpsamplingBilinear initWithDevice_integerScaleFactorX_integerScaleFactorY_alignCornersSelector device integerScaleFactorX integerScaleFactorY alignCorners

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:integerScaleFactorX:integerScaleFactorY:@
initWithDevice_integerScaleFactorX_integerScaleFactorYSelector :: Selector '[RawId, CULong, CULong] (Id MPSCNNUpsamplingBilinear)
initWithDevice_integerScaleFactorX_integerScaleFactorYSelector = mkSelector "initWithDevice:integerScaleFactorX:integerScaleFactorY:"

-- | @Selector@ for @initWithDevice:integerScaleFactorX:integerScaleFactorY:alignCorners:@
initWithDevice_integerScaleFactorX_integerScaleFactorY_alignCornersSelector :: Selector '[RawId, CULong, CULong, Bool] (Id MPSCNNUpsamplingBilinear)
initWithDevice_integerScaleFactorX_integerScaleFactorY_alignCornersSelector = mkSelector "initWithDevice:integerScaleFactorX:integerScaleFactorY:alignCorners:"

