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
initWithDevice_integerScaleFactorX_integerScaleFactorY mpscnnUpsamplingBilinear  device integerScaleFactorX integerScaleFactorY =
  sendMsg mpscnnUpsamplingBilinear (mkSelector "initWithDevice:integerScaleFactorX:integerScaleFactorY:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral integerScaleFactorX), argCULong (fromIntegral integerScaleFactorY)] >>= ownedObject . castPtr

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
initWithDevice_integerScaleFactorX_integerScaleFactorY_alignCorners mpscnnUpsamplingBilinear  device integerScaleFactorX integerScaleFactorY alignCorners =
  sendMsg mpscnnUpsamplingBilinear (mkSelector "initWithDevice:integerScaleFactorX:integerScaleFactorY:alignCorners:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral integerScaleFactorX), argCULong (fromIntegral integerScaleFactorY), argCULong (if alignCorners then 1 else 0)] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:integerScaleFactorX:integerScaleFactorY:@
initWithDevice_integerScaleFactorX_integerScaleFactorYSelector :: Selector
initWithDevice_integerScaleFactorX_integerScaleFactorYSelector = mkSelector "initWithDevice:integerScaleFactorX:integerScaleFactorY:"

-- | @Selector@ for @initWithDevice:integerScaleFactorX:integerScaleFactorY:alignCorners:@
initWithDevice_integerScaleFactorX_integerScaleFactorY_alignCornersSelector :: Selector
initWithDevice_integerScaleFactorX_integerScaleFactorY_alignCornersSelector = mkSelector "initWithDevice:integerScaleFactorX:integerScaleFactorY:alignCorners:"

