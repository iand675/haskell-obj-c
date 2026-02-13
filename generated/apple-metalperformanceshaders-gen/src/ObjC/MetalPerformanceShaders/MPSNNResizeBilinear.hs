{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNResizeBilinear
--
-- This depends on Metal.framework
--
-- The MPSNNResizeBilinear filter resizes the source image  using bilinear interpolation to              a destination whose dimensions are given by resizeWidth and resizeHeight
--
-- The number of output feature channels remains the same as the number of input feature              channels.
--
-- Generated bindings for @MPSNNResizeBilinear@.
module ObjC.MetalPerformanceShaders.MPSNNResizeBilinear
  ( MPSNNResizeBilinear
  , IsMPSNNResizeBilinear(..)
  , initWithDevice
  , initWithDevice_resizeWidth_resizeHeight_alignCorners
  , initWithCoder_device
  , resizeWidth
  , resizeHeight
  , alignCorners
  , alignCornersSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_resizeWidth_resizeHeight_alignCornersSelector
  , resizeHeightSelector
  , resizeWidthSelector


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
initWithDevice :: IsMPSNNResizeBilinear mpsnnResizeBilinear => mpsnnResizeBilinear -> RawId -> IO (Id MPSNNResizeBilinear)
initWithDevice mpsnnResizeBilinear device =
  sendOwnedMessage mpsnnResizeBilinear initWithDeviceSelector device

-- | Initialize the resize bilinear filter.
--
-- @device@ — The device the filter will run on.
--
-- @resizeWidth@ — The destination resize width in pixels
--
-- @resizeHeight@ — The destination resize height in pixels
--
-- @alignCorners@ — Specifier whether the centers of the 4 corner pixels of the input and output regions are aligned,                                      preserving the values at the corner pixels.
--
-- Returns: A valid MPSNNResizeBilinear object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:resizeWidth:resizeHeight:alignCorners:@
initWithDevice_resizeWidth_resizeHeight_alignCorners :: IsMPSNNResizeBilinear mpsnnResizeBilinear => mpsnnResizeBilinear -> RawId -> CULong -> CULong -> Bool -> IO (Id MPSNNResizeBilinear)
initWithDevice_resizeWidth_resizeHeight_alignCorners mpsnnResizeBilinear device resizeWidth resizeHeight alignCorners =
  sendOwnedMessage mpsnnResizeBilinear initWithDevice_resizeWidth_resizeHeight_alignCornersSelector device resizeWidth resizeHeight alignCorners

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSNNResizeBilinear
--
-- @device@ — The MTLDevice on which to make the MPSNNResizeBilinear
--
-- Returns: A new MPSNNResizeBilinear object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNResizeBilinear mpsnnResizeBilinear, IsNSCoder aDecoder) => mpsnnResizeBilinear -> aDecoder -> RawId -> IO (Id MPSNNResizeBilinear)
initWithCoder_device mpsnnResizeBilinear aDecoder device =
  sendOwnedMessage mpsnnResizeBilinear initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | resizeWidth
--
-- The resize width.
--
-- ObjC selector: @- resizeWidth@
resizeWidth :: IsMPSNNResizeBilinear mpsnnResizeBilinear => mpsnnResizeBilinear -> IO CULong
resizeWidth mpsnnResizeBilinear =
  sendMessage mpsnnResizeBilinear resizeWidthSelector

-- | resizeHeight
--
-- The resize height.
--
-- ObjC selector: @- resizeHeight@
resizeHeight :: IsMPSNNResizeBilinear mpsnnResizeBilinear => mpsnnResizeBilinear -> IO CULong
resizeHeight mpsnnResizeBilinear =
  sendMessage mpsnnResizeBilinear resizeHeightSelector

-- | alignCorners
--
-- If YES, the centers of the 4 corner pixels of the input and output regions are aligned,              preserving the values at the corner pixels.              The default is NO.
--
-- ObjC selector: @- alignCorners@
alignCorners :: IsMPSNNResizeBilinear mpsnnResizeBilinear => mpsnnResizeBilinear -> IO Bool
alignCorners mpsnnResizeBilinear =
  sendMessage mpsnnResizeBilinear alignCornersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNResizeBilinear)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:resizeWidth:resizeHeight:alignCorners:@
initWithDevice_resizeWidth_resizeHeight_alignCornersSelector :: Selector '[RawId, CULong, CULong, Bool] (Id MPSNNResizeBilinear)
initWithDevice_resizeWidth_resizeHeight_alignCornersSelector = mkSelector "initWithDevice:resizeWidth:resizeHeight:alignCorners:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNResizeBilinear)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @resizeWidth@
resizeWidthSelector :: Selector '[] CULong
resizeWidthSelector = mkSelector "resizeWidth"

-- | @Selector@ for @resizeHeight@
resizeHeightSelector :: Selector '[] CULong
resizeHeightSelector = mkSelector "resizeHeight"

-- | @Selector@ for @alignCorners@
alignCornersSelector :: Selector '[] Bool
alignCornersSelector = mkSelector "alignCorners"

