{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageEuclideanDistanceTransform
--
-- Perform a Euclidean Distance Transform
--
-- Generated bindings for @MPSImageEuclideanDistanceTransform@.
module ObjC.MetalPerformanceShaders.MPSImageEuclideanDistanceTransform
  ( MPSImageEuclideanDistanceTransform
  , IsMPSImageEuclideanDistanceTransform(..)
  , initWithDevice
  , initWithCoder_device
  , searchLimitRadius
  , setSearchLimitRadius
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , searchLimitRadiusSelector
  , setSearchLimitRadiusSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Specifies information to apply the statistics min-max operation on an image.
--
-- @device@ — The device the filter will run on
--
-- Returns: A valid MPSImageEuclideanDistanceTransform object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSImageEuclideanDistanceTransform mpsImageEuclideanDistanceTransform => mpsImageEuclideanDistanceTransform -> RawId -> IO (Id MPSImageEuclideanDistanceTransform)
initWithDevice mpsImageEuclideanDistanceTransform device =
  sendOwnedMessage mpsImageEuclideanDistanceTransform initWithDeviceSelector device

-- | NSSecureCoding compatability
--
-- While the standard NSSecureCoding/NSCoding method              -initWithCoder: should work, since the file can't              know which device your data is allocated on, we              have to guess and may guess incorrectly.  To avoid              that problem, use initWithCoder:device instead.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSKernel
--
-- @device@ — The MTLDevice on which to make the MPSKernel
--
-- Returns: A new MPSKernel object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSImageEuclideanDistanceTransform mpsImageEuclideanDistanceTransform, IsNSCoder aDecoder) => mpsImageEuclideanDistanceTransform -> aDecoder -> RawId -> IO (Id MPSImageEuclideanDistanceTransform)
initWithCoder_device mpsImageEuclideanDistanceTransform aDecoder device =
  sendOwnedMessage mpsImageEuclideanDistanceTransform initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | searchLimitRadius
--
-- Defines a search scope size around output pixel to limit closest non-zero pixel search. Optional variable.
--
-- When the non-zeroes in the input image are on average very far away from each other (ie. the distances are large),              the distance calculation algorithm has to work harder to find the closest pixel. If you don't care about getting exact              results beyond a certain distance you can use this property to limit the search space and speed up the kernels.              In case there are no non-zero pixels within this search scope around the output pixel, then the output value will              be some number that is larger than this search limit. Normally you should be fine with the default value of FLT_MAX,              which results in the exact EDT, so use this only if you need additional performance.              Typical good values are: 32, 64, 96, 128.              Default: FLT_MAX
--
-- ObjC selector: @- searchLimitRadius@
searchLimitRadius :: IsMPSImageEuclideanDistanceTransform mpsImageEuclideanDistanceTransform => mpsImageEuclideanDistanceTransform -> IO CFloat
searchLimitRadius mpsImageEuclideanDistanceTransform =
  sendMessage mpsImageEuclideanDistanceTransform searchLimitRadiusSelector

-- | searchLimitRadius
--
-- Defines a search scope size around output pixel to limit closest non-zero pixel search. Optional variable.
--
-- When the non-zeroes in the input image are on average very far away from each other (ie. the distances are large),              the distance calculation algorithm has to work harder to find the closest pixel. If you don't care about getting exact              results beyond a certain distance you can use this property to limit the search space and speed up the kernels.              In case there are no non-zero pixels within this search scope around the output pixel, then the output value will              be some number that is larger than this search limit. Normally you should be fine with the default value of FLT_MAX,              which results in the exact EDT, so use this only if you need additional performance.              Typical good values are: 32, 64, 96, 128.              Default: FLT_MAX
--
-- ObjC selector: @- setSearchLimitRadius:@
setSearchLimitRadius :: IsMPSImageEuclideanDistanceTransform mpsImageEuclideanDistanceTransform => mpsImageEuclideanDistanceTransform -> CFloat -> IO ()
setSearchLimitRadius mpsImageEuclideanDistanceTransform value =
  sendMessage mpsImageEuclideanDistanceTransform setSearchLimitRadiusSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImageEuclideanDistanceTransform)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSImageEuclideanDistanceTransform)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @searchLimitRadius@
searchLimitRadiusSelector :: Selector '[] CFloat
searchLimitRadiusSelector = mkSelector "searchLimitRadius"

-- | @Selector@ for @setSearchLimitRadius:@
setSearchLimitRadiusSelector :: Selector '[CFloat] ()
setSearchLimitRadiusSelector = mkSelector "setSearchLimitRadius:"

