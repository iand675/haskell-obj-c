{-# LANGUAGE DataKinds #-}
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
  , alphaSelector
  , betaSelector
  , deltaSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_kernelWidth_kernelHeightSelector
  , setAlphaSelector
  , setBetaSelector
  , setDeltaSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithDevice_kernelWidth_kernelHeight mpscnnSpatialNormalization device kernelWidth kernelHeight =
  sendOwnedMessage mpscnnSpatialNormalization initWithDevice_kernelWidth_kernelHeightSelector device kernelWidth kernelHeight

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
initWithCoder_device mpscnnSpatialNormalization aDecoder device =
  sendOwnedMessage mpscnnSpatialNormalization initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNSpatialNormalization mpscnnSpatialNormalization => mpscnnSpatialNormalization -> RawId -> IO (Id MPSCNNSpatialNormalization)
initWithDevice mpscnnSpatialNormalization device =
  sendOwnedMessage mpscnnSpatialNormalization initWithDeviceSelector device

-- | alpha
--
-- The value of alpha.  Default is 1.0. Must be non-negative.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSCNNSpatialNormalization mpscnnSpatialNormalization => mpscnnSpatialNormalization -> IO CFloat
alpha mpscnnSpatialNormalization =
  sendMessage mpscnnSpatialNormalization alphaSelector

-- | alpha
--
-- The value of alpha.  Default is 1.0. Must be non-negative.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSCNNSpatialNormalization mpscnnSpatialNormalization => mpscnnSpatialNormalization -> CFloat -> IO ()
setAlpha mpscnnSpatialNormalization value =
  sendMessage mpscnnSpatialNormalization setAlphaSelector value

-- | beta
--
-- The value of beta.  Default is 5.0
--
-- ObjC selector: @- beta@
beta :: IsMPSCNNSpatialNormalization mpscnnSpatialNormalization => mpscnnSpatialNormalization -> IO CFloat
beta mpscnnSpatialNormalization =
  sendMessage mpscnnSpatialNormalization betaSelector

-- | beta
--
-- The value of beta.  Default is 5.0
--
-- ObjC selector: @- setBeta:@
setBeta :: IsMPSCNNSpatialNormalization mpscnnSpatialNormalization => mpscnnSpatialNormalization -> CFloat -> IO ()
setBeta mpscnnSpatialNormalization value =
  sendMessage mpscnnSpatialNormalization setBetaSelector value

-- | delta
--
-- The value of delta.  Default is 1.0
--
-- ObjC selector: @- delta@
delta :: IsMPSCNNSpatialNormalization mpscnnSpatialNormalization => mpscnnSpatialNormalization -> IO CFloat
delta mpscnnSpatialNormalization =
  sendMessage mpscnnSpatialNormalization deltaSelector

-- | delta
--
-- The value of delta.  Default is 1.0
--
-- ObjC selector: @- setDelta:@
setDelta :: IsMPSCNNSpatialNormalization mpscnnSpatialNormalization => mpscnnSpatialNormalization -> CFloat -> IO ()
setDelta mpscnnSpatialNormalization value =
  sendMessage mpscnnSpatialNormalization setDeltaSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:@
initWithDevice_kernelWidth_kernelHeightSelector :: Selector '[RawId, CULong, CULong] (Id MPSCNNSpatialNormalization)
initWithDevice_kernelWidth_kernelHeightSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNSpatialNormalization)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNSpatialNormalization)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector '[] CFloat
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @setAlpha:@
setAlphaSelector :: Selector '[CFloat] ()
setAlphaSelector = mkSelector "setAlpha:"

-- | @Selector@ for @beta@
betaSelector :: Selector '[] CFloat
betaSelector = mkSelector "beta"

-- | @Selector@ for @setBeta:@
setBetaSelector :: Selector '[CFloat] ()
setBetaSelector = mkSelector "setBeta:"

-- | @Selector@ for @delta@
deltaSelector :: Selector '[] CFloat
deltaSelector = mkSelector "delta"

-- | @Selector@ for @setDelta:@
setDeltaSelector :: Selector '[CFloat] ()
setDeltaSelector = mkSelector "setDelta:"

