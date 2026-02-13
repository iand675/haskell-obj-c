{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNUpsamplingNearest
--
-- This depends on Metal.framework.
--
-- Specifies the nearest spatial upsampling filter.
--
-- Generated bindings for @MPSCNNUpsamplingNearest@.
module ObjC.MetalPerformanceShaders.MPSCNNUpsamplingNearest
  ( MPSCNNUpsamplingNearest
  , IsMPSCNNUpsamplingNearest(..)
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
-- @integerScaleFactorX@ — The upsampling factor for the x dimension.
--
-- @integerScaleFactorY@ — The upsampling factor for the y dimension.
--
-- Returns: A valid MPSCNNUpsamplingNearest object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:integerScaleFactorX:integerScaleFactorY:@
initWithDevice_integerScaleFactorX_integerScaleFactorY :: IsMPSCNNUpsamplingNearest mpscnnUpsamplingNearest => mpscnnUpsamplingNearest -> RawId -> CULong -> CULong -> IO (Id MPSCNNUpsamplingNearest)
initWithDevice_integerScaleFactorX_integerScaleFactorY mpscnnUpsamplingNearest device integerScaleFactorX integerScaleFactorY =
  sendOwnedMessage mpscnnUpsamplingNearest initWithDevice_integerScaleFactorX_integerScaleFactorYSelector device integerScaleFactorX integerScaleFactorY

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:integerScaleFactorX:integerScaleFactorY:@
initWithDevice_integerScaleFactorX_integerScaleFactorYSelector :: Selector '[RawId, CULong, CULong] (Id MPSCNNUpsamplingNearest)
initWithDevice_integerScaleFactorX_integerScaleFactorYSelector = mkSelector "initWithDevice:integerScaleFactorX:integerScaleFactorY:"

