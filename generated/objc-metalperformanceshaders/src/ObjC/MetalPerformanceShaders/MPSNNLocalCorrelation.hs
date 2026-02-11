{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNLocalCorrelation
--
-- The MPSNNLocalCorrelation filter computes the correlation between two images locally with a              varying offset on x-y plane between the two source images (controlled by the window and              stride properties) and the end result is summed over the feature channels. The results are              stored in the different feature channels of the destination image, ordered such that the offset              in the x direction is the faster running index.
--
-- Given two images A and B, the output image has (2*windowInX + 1)*(2*windowInY + 1)              feature channels, with each feature channel computed as:                                  O(x, y, f(m, n)) = sum_z{A(x, y, z) * B(x + M[m], y + N[n], z)}              where m runs from {0, 1, ... , (2*windowInX)}, n runs from {0, 1, ... , (2*windowInY)},              f(m, n) = n * (2*windowInY + 1) + m,              M = {-windowInX*strideInX, (-windowInX + 1)*strideInX,  ... 0 ... , (windowInX - 1)*strideInX, windowInX*strideInX},              N = {-windowInY*strideInY, (-windowInY + 1)*strideInY,  ... 0 ... , (windowInY - 1)*strideInY, windowInX*strideInY}
--
-- Generated bindings for @MPSNNLocalCorrelation@.
module ObjC.MetalPerformanceShaders.MPSNNLocalCorrelation
  ( MPSNNLocalCorrelation
  , IsMPSNNLocalCorrelation(..)
  , initWithDevice
  , initWithDevice_windowInX_windowInY_strideInX_strideInY
  , initWithCoder_device
  , windowInX
  , setWindowInX
  , windowInY
  , setWindowInY
  , strideInX
  , setStrideInX
  , strideInY
  , setStrideInY
  , initWithDeviceSelector
  , initWithDevice_windowInX_windowInY_strideInX_strideInYSelector
  , initWithCoder_deviceSelector
  , windowInXSelector
  , setWindowInXSelector
  , windowInYSelector
  , setWindowInYSelector
  , strideInXSelector
  , setStrideInXSelector
  , strideInYSelector
  , setStrideInYSelector


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

-- | Initialize the MPSNNLocalCorrelation filter with default property values.
--
-- @device@ — The device the filter will run on
--
-- Returns: A valid MPSNNReduceLocalCorrelation object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNLocalCorrelation mpsnnLocalCorrelation => mpsnnLocalCorrelation -> RawId -> IO (Id MPSNNLocalCorrelation)
initWithDevice mpsnnLocalCorrelation  device =
  sendMsg mpsnnLocalCorrelation (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Specifies information to apply the local correlation operation on an image.
--
-- @device@ — The device the filter will run on
--
-- @windowInX@ — Specifies a symmetric window around 0 for offsetting                                  the secondary source in the x dimension.
--
-- @windowInY@ — Specifies a symmetric window around 0 for offsetting                                  the secondary source in the y dimension.
--
-- @strideInX@ — Specifies the stride for the offset in the x dimension.
--
-- @strideInY@ — Specifies the stride for the offset in the y dimension.
--
-- Returns: A valid MPSNNReduceLocalCorrelation object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:windowInX:windowInY:strideInX:strideInY:@
initWithDevice_windowInX_windowInY_strideInX_strideInY :: IsMPSNNLocalCorrelation mpsnnLocalCorrelation => mpsnnLocalCorrelation -> RawId -> CULong -> CULong -> CULong -> CULong -> IO (Id MPSNNLocalCorrelation)
initWithDevice_windowInX_windowInY_strideInX_strideInY mpsnnLocalCorrelation  device windowInX windowInY strideInX strideInY =
  sendMsg mpsnnLocalCorrelation (mkSelector "initWithDevice:windowInX:windowInY:strideInX:strideInY:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral windowInX), argCULong (fromIntegral windowInY), argCULong (fromIntegral strideInX), argCULong (fromIntegral strideInY)] >>= ownedObject . castPtr

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPooling
--
-- @device@ — The MTLDevice on which to make the MPSCNNPooling
--
-- Returns: A new MPSCNNPooling object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNLocalCorrelation mpsnnLocalCorrelation, IsNSCoder aDecoder) => mpsnnLocalCorrelation -> aDecoder -> RawId -> IO (Id MPSNNLocalCorrelation)
initWithCoder_device mpsnnLocalCorrelation  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsnnLocalCorrelation (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Specifies a symmetric window around 0 for offsetting the secondary source in the x dimension.
--
-- The default value for windowInX is 0.
--
-- ObjC selector: @- windowInX@
windowInX :: IsMPSNNLocalCorrelation mpsnnLocalCorrelation => mpsnnLocalCorrelation -> IO CULong
windowInX mpsnnLocalCorrelation  =
  sendMsg mpsnnLocalCorrelation (mkSelector "windowInX") retCULong []

-- | Specifies a symmetric window around 0 for offsetting the secondary source in the x dimension.
--
-- The default value for windowInX is 0.
--
-- ObjC selector: @- setWindowInX:@
setWindowInX :: IsMPSNNLocalCorrelation mpsnnLocalCorrelation => mpsnnLocalCorrelation -> CULong -> IO ()
setWindowInX mpsnnLocalCorrelation  value =
  sendMsg mpsnnLocalCorrelation (mkSelector "setWindowInX:") retVoid [argCULong (fromIntegral value)]

-- | Specifies a symmetric window around 0 for offsetting the secondary source in the y dimension.
--
-- The default value for windowInY is 0.
--
-- ObjC selector: @- windowInY@
windowInY :: IsMPSNNLocalCorrelation mpsnnLocalCorrelation => mpsnnLocalCorrelation -> IO CULong
windowInY mpsnnLocalCorrelation  =
  sendMsg mpsnnLocalCorrelation (mkSelector "windowInY") retCULong []

-- | Specifies a symmetric window around 0 for offsetting the secondary source in the y dimension.
--
-- The default value for windowInY is 0.
--
-- ObjC selector: @- setWindowInY:@
setWindowInY :: IsMPSNNLocalCorrelation mpsnnLocalCorrelation => mpsnnLocalCorrelation -> CULong -> IO ()
setWindowInY mpsnnLocalCorrelation  value =
  sendMsg mpsnnLocalCorrelation (mkSelector "setWindowInY:") retVoid [argCULong (fromIntegral value)]

-- | Specifies the stride for the offset in the x dimension.
--
-- strideInX must be > 0. The default value for strideInX is 1.
--
-- ObjC selector: @- strideInX@
strideInX :: IsMPSNNLocalCorrelation mpsnnLocalCorrelation => mpsnnLocalCorrelation -> IO CULong
strideInX mpsnnLocalCorrelation  =
  sendMsg mpsnnLocalCorrelation (mkSelector "strideInX") retCULong []

-- | Specifies the stride for the offset in the x dimension.
--
-- strideInX must be > 0. The default value for strideInX is 1.
--
-- ObjC selector: @- setStrideInX:@
setStrideInX :: IsMPSNNLocalCorrelation mpsnnLocalCorrelation => mpsnnLocalCorrelation -> CULong -> IO ()
setStrideInX mpsnnLocalCorrelation  value =
  sendMsg mpsnnLocalCorrelation (mkSelector "setStrideInX:") retVoid [argCULong (fromIntegral value)]

-- | Specifies the stride for the offset in the y dimension.
--
-- strideInY must be > 0. The default value for strideInY is 1.
--
-- ObjC selector: @- strideInY@
strideInY :: IsMPSNNLocalCorrelation mpsnnLocalCorrelation => mpsnnLocalCorrelation -> IO CULong
strideInY mpsnnLocalCorrelation  =
  sendMsg mpsnnLocalCorrelation (mkSelector "strideInY") retCULong []

-- | Specifies the stride for the offset in the y dimension.
--
-- strideInY must be > 0. The default value for strideInY is 1.
--
-- ObjC selector: @- setStrideInY:@
setStrideInY :: IsMPSNNLocalCorrelation mpsnnLocalCorrelation => mpsnnLocalCorrelation -> CULong -> IO ()
setStrideInY mpsnnLocalCorrelation  value =
  sendMsg mpsnnLocalCorrelation (mkSelector "setStrideInY:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:windowInX:windowInY:strideInX:strideInY:@
initWithDevice_windowInX_windowInY_strideInX_strideInYSelector :: Selector
initWithDevice_windowInX_windowInY_strideInX_strideInYSelector = mkSelector "initWithDevice:windowInX:windowInY:strideInX:strideInY:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @windowInX@
windowInXSelector :: Selector
windowInXSelector = mkSelector "windowInX"

-- | @Selector@ for @setWindowInX:@
setWindowInXSelector :: Selector
setWindowInXSelector = mkSelector "setWindowInX:"

-- | @Selector@ for @windowInY@
windowInYSelector :: Selector
windowInYSelector = mkSelector "windowInY"

-- | @Selector@ for @setWindowInY:@
setWindowInYSelector :: Selector
setWindowInYSelector = mkSelector "setWindowInY:"

-- | @Selector@ for @strideInX@
strideInXSelector :: Selector
strideInXSelector = mkSelector "strideInX"

-- | @Selector@ for @setStrideInX:@
setStrideInXSelector :: Selector
setStrideInXSelector = mkSelector "setStrideInX:"

-- | @Selector@ for @strideInY@
strideInYSelector :: Selector
strideInYSelector = mkSelector "strideInY"

-- | @Selector@ for @setStrideInY:@
setStrideInYSelector :: Selector
setStrideInYSelector = mkSelector "setStrideInY:"

