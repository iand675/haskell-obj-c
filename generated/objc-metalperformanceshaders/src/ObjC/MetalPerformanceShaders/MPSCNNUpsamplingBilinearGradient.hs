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
initWithDevice_integerScaleFactorX_integerScaleFactorY mpscnnUpsamplingBilinearGradient  device integerScaleFactorX integerScaleFactorY =
  sendMsg mpscnnUpsamplingBilinearGradient (mkSelector "initWithDevice:integerScaleFactorX:integerScaleFactorY:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral integerScaleFactorX), argCULong (fromIntegral integerScaleFactorY)] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:integerScaleFactorX:integerScaleFactorY:@
initWithDevice_integerScaleFactorX_integerScaleFactorYSelector :: Selector
initWithDevice_integerScaleFactorX_integerScaleFactorYSelector = mkSelector "initWithDevice:integerScaleFactorX:integerScaleFactorY:"

