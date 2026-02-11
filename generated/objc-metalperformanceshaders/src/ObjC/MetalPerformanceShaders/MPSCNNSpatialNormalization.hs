{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNSpatialNormalization
--
-- This depends on Metal.framework
--
-- Specifies the spatial normalization filter.              The spatial normalization for a feature channel applies the filter over local regions which extend              spatially, but are in separate feature channels (i.e., they have shape 1 x kernelWidth x kernelHeight).              For each feature channel, the function computes the sum of squares of X inside each rectangle, N2(i,j).              It then divides each element of X as follows:                  Y(i,j) = X(i,j) / (delta + alpha/(kw*kh) * N2(i,j))^beta,              where kw and kh are the kernelWidth and the kernelHeight.              It is the end-users responsibility to ensure that the combination of the              parameters delta and alpha does not result in a situation where the denominator              becomes zero - in such situations the resulting pixel-value is undefined.
--
-- Generated bindings for @MPSCNNSpatialNormalization@.
module ObjC.MetalPerformanceShaders.MPSCNNSpatialNormalization
  ( MPSCNNSpatialNormalization
  , IsMPSCNNSpatialNormalization(..)
  , initWithDevice_kernelWidth_kernelHeight
  , initWithCoder_device
  , initWithDevice
  , alpha
  , setAlpha
  , beta
  , setBeta
  , delta
  , setDelta
  , initWithDevice_kernelWidth_kernelHeightSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , alphaSelector
  , setAlphaSelector
  , betaSelector
  , setBetaSelector
  , deltaSelector
  , setDeltaSelector


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

-- | Initialize a spatial normalization filter
--
-- @device@ — The device the filter will run on
--
-- @kernelWidth@ — The width of the kernel
--
-- @kernelHeight@ — The height of the kernel
--
-- Returns: A valid MPSCNNSpatialNormalization object or nil, if failure.
--
-- NOTE:  For now, kernelWidth must be equal to kernelHeight
--
-- ObjC selector: @- initWithDevice:kernelWidth:kernelHeight:@
initWithDevice_kernelWidth_kernelHeight :: IsMPSCNNSpatialNormalization mpscnnSpatialNormalization => mpscnnSpatialNormalization -> RawId -> CULong -> CULong -> IO (Id MPSCNNSpatialNormalization)
initWithDevice_kernelWidth_kernelHeight mpscnnSpatialNormalization  device kernelWidth kernelHeight =
  sendMsg mpscnnSpatialNormalization (mkSelector "initWithDevice:kernelWidth:kernelHeight:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight)] >>= ownedObject . castPtr

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
initWithCoder_device :: (IsMPSCNNSpatialNormalization mpscnnSpatialNormalization, IsNSCoder aDecoder) => mpscnnSpatialNormalization -> aDecoder -> RawId -> IO (Id MPSCNNSpatialNormalization)
initWithCoder_device mpscnnSpatialNormalization  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpscnnSpatialNormalization (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNSpatialNormalization mpscnnSpatialNormalization => mpscnnSpatialNormalization -> RawId -> IO (Id MPSCNNSpatialNormalization)
initWithDevice mpscnnSpatialNormalization  device =
  sendMsg mpscnnSpatialNormalization (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | alpha
--
-- The value of alpha.  Default is 1.0. Must be non-negative.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSCNNSpatialNormalization mpscnnSpatialNormalization => mpscnnSpatialNormalization -> IO CFloat
alpha mpscnnSpatialNormalization  =
  sendMsg mpscnnSpatialNormalization (mkSelector "alpha") retCFloat []

-- | alpha
--
-- The value of alpha.  Default is 1.0. Must be non-negative.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSCNNSpatialNormalization mpscnnSpatialNormalization => mpscnnSpatialNormalization -> CFloat -> IO ()
setAlpha mpscnnSpatialNormalization  value =
  sendMsg mpscnnSpatialNormalization (mkSelector "setAlpha:") retVoid [argCFloat (fromIntegral value)]

-- | beta
--
-- The value of beta.  Default is 5.0
--
-- ObjC selector: @- beta@
beta :: IsMPSCNNSpatialNormalization mpscnnSpatialNormalization => mpscnnSpatialNormalization -> IO CFloat
beta mpscnnSpatialNormalization  =
  sendMsg mpscnnSpatialNormalization (mkSelector "beta") retCFloat []

-- | beta
--
-- The value of beta.  Default is 5.0
--
-- ObjC selector: @- setBeta:@
setBeta :: IsMPSCNNSpatialNormalization mpscnnSpatialNormalization => mpscnnSpatialNormalization -> CFloat -> IO ()
setBeta mpscnnSpatialNormalization  value =
  sendMsg mpscnnSpatialNormalization (mkSelector "setBeta:") retVoid [argCFloat (fromIntegral value)]

-- | delta
--
-- The value of delta.  Default is 1.0
--
-- ObjC selector: @- delta@
delta :: IsMPSCNNSpatialNormalization mpscnnSpatialNormalization => mpscnnSpatialNormalization -> IO CFloat
delta mpscnnSpatialNormalization  =
  sendMsg mpscnnSpatialNormalization (mkSelector "delta") retCFloat []

-- | delta
--
-- The value of delta.  Default is 1.0
--
-- ObjC selector: @- setDelta:@
setDelta :: IsMPSCNNSpatialNormalization mpscnnSpatialNormalization => mpscnnSpatialNormalization -> CFloat -> IO ()
setDelta mpscnnSpatialNormalization  value =
  sendMsg mpscnnSpatialNormalization (mkSelector "setDelta:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:@
initWithDevice_kernelWidth_kernelHeightSelector :: Selector
initWithDevice_kernelWidth_kernelHeightSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @setAlpha:@
setAlphaSelector :: Selector
setAlphaSelector = mkSelector "setAlpha:"

-- | @Selector@ for @beta@
betaSelector :: Selector
betaSelector = mkSelector "beta"

-- | @Selector@ for @setBeta:@
setBetaSelector :: Selector
setBetaSelector = mkSelector "setBeta:"

-- | @Selector@ for @delta@
deltaSelector :: Selector
deltaSelector = mkSelector "delta"

-- | @Selector@ for @setDelta:@
setDeltaSelector :: Selector
setDeltaSelector = mkSelector "setDelta:"

