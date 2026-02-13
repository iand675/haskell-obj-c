{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNCropAndResizeBilinear
--
-- This depends on Metal.framework
--
-- The MPSNNCropAndResizeBilinear filter resizes the source image  using bilinear interpolation to              a destination whose dimensions are given by resizeWidth and resizeHeight
--
-- The number of output feature channels remains the same as the number of input feature              channels.
--
-- Generated bindings for @MPSNNCropAndResizeBilinear@.
module ObjC.MetalPerformanceShaders.MPSNNCropAndResizeBilinear
  ( MPSNNCropAndResizeBilinear
  , IsMPSNNCropAndResizeBilinear(..)
  , initWithDevice
  , initWithDevice_resizeWidth_resizeHeight_numberOfRegions_regions
  , initWithCoder_device
  , resizeWidth
  , resizeHeight
  , numberOfRegions
  , regions
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_resizeWidth_resizeHeight_numberOfRegions_regionsSelector
  , numberOfRegionsSelector
  , regionsSelector
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
import ObjC.MetalPerformanceShaders.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:@
initWithDevice :: IsMPSNNCropAndResizeBilinear mpsnnCropAndResizeBilinear => mpsnnCropAndResizeBilinear -> RawId -> IO (Id MPSNNCropAndResizeBilinear)
initWithDevice mpsnnCropAndResizeBilinear device =
  sendOwnedMessage mpsnnCropAndResizeBilinear initWithDeviceSelector device

-- | Initialize the crop and resize bilinear filter.
--
-- @device@ — The device the filter will run on.
--
-- @resizeWidth@ — The destination resize width in pixels
--
-- @resizeHeight@ — The destination resize height in pixels
--
-- @numberOfRegions@ — Specifies the number of bounding box i.e. regions to resize
--
-- @regions@ — This is a pointer to "numberOfRegions" boxes which specify the locations in the                                      source image to use for each box/region to perform the resize operation.
--
-- Returns: A valid MPSNNCropAndResizeBilinear object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:resizeWidth:resizeHeight:numberOfRegions:regions:@
initWithDevice_resizeWidth_resizeHeight_numberOfRegions_regions :: IsMPSNNCropAndResizeBilinear mpsnnCropAndResizeBilinear => mpsnnCropAndResizeBilinear -> RawId -> CULong -> CULong -> CULong -> Const (Ptr MPSRegion) -> IO (Id MPSNNCropAndResizeBilinear)
initWithDevice_resizeWidth_resizeHeight_numberOfRegions_regions mpsnnCropAndResizeBilinear device resizeWidth resizeHeight numberOfRegions regions =
  sendOwnedMessage mpsnnCropAndResizeBilinear initWithDevice_resizeWidth_resizeHeight_numberOfRegions_regionsSelector device resizeWidth resizeHeight numberOfRegions regions

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSNNCropAndResizeBilinear
--
-- @device@ — The MTLDevice on which to make the MPSNNCropAndResizeBilinear
--
-- Returns: A new MPSNNResizeBilinear object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNCropAndResizeBilinear mpsnnCropAndResizeBilinear, IsNSCoder aDecoder) => mpsnnCropAndResizeBilinear -> aDecoder -> RawId -> IO (Id MPSNNCropAndResizeBilinear)
initWithCoder_device mpsnnCropAndResizeBilinear aDecoder device =
  sendOwnedMessage mpsnnCropAndResizeBilinear initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | resizeWidth
--
-- The resize width.
--
-- ObjC selector: @- resizeWidth@
resizeWidth :: IsMPSNNCropAndResizeBilinear mpsnnCropAndResizeBilinear => mpsnnCropAndResizeBilinear -> IO CULong
resizeWidth mpsnnCropAndResizeBilinear =
  sendMessage mpsnnCropAndResizeBilinear resizeWidthSelector

-- | resizeHeight
--
-- The resize height.
--
-- ObjC selector: @- resizeHeight@
resizeHeight :: IsMPSNNCropAndResizeBilinear mpsnnCropAndResizeBilinear => mpsnnCropAndResizeBilinear -> IO CULong
resizeHeight mpsnnCropAndResizeBilinear =
  sendMessage mpsnnCropAndResizeBilinear resizeHeightSelector

-- | numberOfRegions
--
-- the number of bounding box i.e. regions to resize.
--
-- ObjC selector: @- numberOfRegions@
numberOfRegions :: IsMPSNNCropAndResizeBilinear mpsnnCropAndResizeBilinear => mpsnnCropAndResizeBilinear -> IO CULong
numberOfRegions mpsnnCropAndResizeBilinear =
  sendMessage mpsnnCropAndResizeBilinear numberOfRegionsSelector

-- | regions
--
-- This is a pointer to "numberOfRegions" boxes which specify the locations in the              source image to use for each box/region to perform the resize operation.              The coordinates specified are normalized values.  A normalized region outside the              [0, 1] range is allowed, in which case we use extrapolation_value to extrapolate              the input image values.
--
-- ObjC selector: @- regions@
regions :: IsMPSNNCropAndResizeBilinear mpsnnCropAndResizeBilinear => mpsnnCropAndResizeBilinear -> IO (Const (Ptr MPSRegion))
regions mpsnnCropAndResizeBilinear =
  sendMessage mpsnnCropAndResizeBilinear regionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNCropAndResizeBilinear)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:resizeWidth:resizeHeight:numberOfRegions:regions:@
initWithDevice_resizeWidth_resizeHeight_numberOfRegions_regionsSelector :: Selector '[RawId, CULong, CULong, CULong, Const (Ptr MPSRegion)] (Id MPSNNCropAndResizeBilinear)
initWithDevice_resizeWidth_resizeHeight_numberOfRegions_regionsSelector = mkSelector "initWithDevice:resizeWidth:resizeHeight:numberOfRegions:regions:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNCropAndResizeBilinear)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @resizeWidth@
resizeWidthSelector :: Selector '[] CULong
resizeWidthSelector = mkSelector "resizeWidth"

-- | @Selector@ for @resizeHeight@
resizeHeightSelector :: Selector '[] CULong
resizeHeightSelector = mkSelector "resizeHeight"

-- | @Selector@ for @numberOfRegions@
numberOfRegionsSelector :: Selector '[] CULong
numberOfRegionsSelector = mkSelector "numberOfRegions"

-- | @Selector@ for @regions@
regionsSelector :: Selector '[] (Const (Ptr MPSRegion))
regionsSelector = mkSelector "regions"

