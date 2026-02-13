{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNUpsamplingBilinearGradient
--
-- This depends on Metal.framework.
--
-- Specifies the bilinear spatial downsampling filter.
--
-- Generated bindings for @MPSCNNUpsamplingBilinearGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNUpsamplingBilinearGradient
  ( MPSCNNUpsamplingBilinearGradient
  , IsMPSCNNUpsamplingBilinearGradient(..)
  , initWithDevice_integerScaleFactorX_integerScaleFactorY
  , initWithDevice_integerScaleFactorX_integerScaleFactorYSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize the bilinear spatial downsampling filter.
--
-- @device@ — The device the filter will run on.
--
-- @integerScaleFactorX@ — The downsampling factor for the x dimension.
--
-- @integerScaleFactorY@ — The downsampling factor for the y dimension.
--
-- Returns: A valid MPSCNNUpsamplingBilinearGradient object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:integerScaleFactorX:integerScaleFactorY:@
initWithDevice_integerScaleFactorX_integerScaleFactorY :: IsMPSCNNUpsamplingBilinearGradient mpscnnUpsamplingBilinearGradient => mpscnnUpsamplingBilinearGradient -> RawId -> CULong -> CULong -> IO (Id MPSCNNUpsamplingBilinearGradient)
initWithDevice_integerScaleFactorX_integerScaleFactorY mpscnnUpsamplingBilinearGradient device integerScaleFactorX integerScaleFactorY =
  sendOwnedMessage mpscnnUpsamplingBilinearGradient initWithDevice_integerScaleFactorX_integerScaleFactorYSelector device integerScaleFactorX integerScaleFactorY

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:integerScaleFactorX:integerScaleFactorY:@
initWithDevice_integerScaleFactorX_integerScaleFactorYSelector :: Selector '[RawId, CULong, CULong] (Id MPSCNNUpsamplingBilinearGradient)
initWithDevice_integerScaleFactorX_integerScaleFactorYSelector = mkSelector "initWithDevice:integerScaleFactorX:integerScaleFactorY:"

