{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNUpsamplingNearestGradient
--
-- This depends on Metal.framework.
--
-- Specifies the nearest spatial downsampling filter.
--
-- Generated bindings for @MPSCNNUpsamplingNearestGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNUpsamplingNearestGradient
  ( MPSCNNUpsamplingNearestGradient
  , IsMPSCNNUpsamplingNearestGradient(..)
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

-- | Initialize the nearest spatial upsampling filter.
--
-- @device@ — The device the filter will run on.
--
-- @integerScaleFactorX@ — The downsampling factor for the x dimension.
--
-- @integerScaleFactorY@ — The downsampling factor for the y dimension.
--
-- Returns: A valid MPSCNNUpsamplingNearestGradient object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:integerScaleFactorX:integerScaleFactorY:@
initWithDevice_integerScaleFactorX_integerScaleFactorY :: IsMPSCNNUpsamplingNearestGradient mpscnnUpsamplingNearestGradient => mpscnnUpsamplingNearestGradient -> RawId -> CULong -> CULong -> IO (Id MPSCNNUpsamplingNearestGradient)
initWithDevice_integerScaleFactorX_integerScaleFactorY mpscnnUpsamplingNearestGradient device integerScaleFactorX integerScaleFactorY =
  sendOwnedMessage mpscnnUpsamplingNearestGradient initWithDevice_integerScaleFactorX_integerScaleFactorYSelector device integerScaleFactorX integerScaleFactorY

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:integerScaleFactorX:integerScaleFactorY:@
initWithDevice_integerScaleFactorX_integerScaleFactorYSelector :: Selector '[RawId, CULong, CULong] (Id MPSCNNUpsamplingNearestGradient)
initWithDevice_integerScaleFactorX_integerScaleFactorYSelector = mkSelector "initWithDevice:integerScaleFactorX:integerScaleFactorY:"

