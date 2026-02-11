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
  , initWithDeviceSelector
  , initWithCoder_deviceSelector
  , searchLimitRadiusSelector
  , setSearchLimitRadiusSelector


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

-- | Specifies information to apply the statistics min-max operation on an image.
--
-- @device@ — The device the filter will run on
--
-- Returns: A valid MPSImageEuclideanDistanceTransform object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSImageEuclideanDistanceTransform mpsImageEuclideanDistanceTransform => mpsImageEuclideanDistanceTransform -> RawId -> IO (Id MPSImageEuclideanDistanceTransform)
initWithDevice mpsImageEuclideanDistanceTransform  device =
  sendMsg mpsImageEuclideanDistanceTransform (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
initWithCoder_device mpsImageEuclideanDistanceTransform  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsImageEuclideanDistanceTransform (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | searchLimitRadius
--
-- Defines a search scope size around output pixel to limit closest non-zero pixel search. Optional variable.
--
-- When the non-zeroes in the input image are on average very far away from each other (ie. the distances are large),              the distance calculation algorithm has to work harder to find the closest pixel. If you don't care about getting exact              results beyond a certain distance you can use this property to limit the search space and speed up the kernels.              In case there are no non-zero pixels within this search scope around the output pixel, then the output value will              be some number that is larger than this search limit. Normally you should be fine with the default value of FLT_MAX,              which results in the exact EDT, so use this only if you need additional performance.              Typical good values are: 32, 64, 96, 128.              Default: FLT_MAX
--
-- ObjC selector: @- searchLimitRadius@
searchLimitRadius :: IsMPSImageEuclideanDistanceTransform mpsImageEuclideanDistanceTransform => mpsImageEuclideanDistanceTransform -> IO CFloat
searchLimitRadius mpsImageEuclideanDistanceTransform  =
  sendMsg mpsImageEuclideanDistanceTransform (mkSelector "searchLimitRadius") retCFloat []

-- | searchLimitRadius
--
-- Defines a search scope size around output pixel to limit closest non-zero pixel search. Optional variable.
--
-- When the non-zeroes in the input image are on average very far away from each other (ie. the distances are large),              the distance calculation algorithm has to work harder to find the closest pixel. If you don't care about getting exact              results beyond a certain distance you can use this property to limit the search space and speed up the kernels.              In case there are no non-zero pixels within this search scope around the output pixel, then the output value will              be some number that is larger than this search limit. Normally you should be fine with the default value of FLT_MAX,              which results in the exact EDT, so use this only if you need additional performance.              Typical good values are: 32, 64, 96, 128.              Default: FLT_MAX
--
-- ObjC selector: @- setSearchLimitRadius:@
setSearchLimitRadius :: IsMPSImageEuclideanDistanceTransform mpsImageEuclideanDistanceTransform => mpsImageEuclideanDistanceTransform -> CFloat -> IO ()
setSearchLimitRadius mpsImageEuclideanDistanceTransform  value =
  sendMsg mpsImageEuclideanDistanceTransform (mkSelector "setSearchLimitRadius:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @searchLimitRadius@
searchLimitRadiusSelector :: Selector
searchLimitRadiusSelector = mkSelector "searchLimitRadius"

-- | @Selector@ for @setSearchLimitRadius:@
setSearchLimitRadiusSelector :: Selector
setSearchLimitRadiusSelector = mkSelector "setSearchLimitRadius:"

