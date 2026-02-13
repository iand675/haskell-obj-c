{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNCrossChannelNormalization
--
-- This depends on Metal.framework
--
-- Specifies the normalization filter across feature channels.               This normalization filter applies the filter to a local region across nearby feature channels,              but with no spatial extent (i.e., they have shape kernelSize x 1 x 1).              The normalized output is given by:                  Y(i,j,k) = X(i,j,k) / L(i,j,k)^beta,              where the normalizing factor is:                  L(i,j,k) = delta + alpha/N * (sum_{q in Q(k)} X(i,j,q)^2, where              N is the kernel size. The window Q(k) itself is defined as:                  Q(k) = [max(0, k-floor(N/2)), min(D-1, k+floor((N-1)/2)], where
--
-- k is the feature channel index (running from 0 to D-1) and              D is the number of feature channels, and alpha, beta and delta are paremeters.              It is the end-users responsibility to ensure that the combination of the              parameters delta and alpha does not result in a situation where the denominator              becomes zero - in such situations the resulting pixel-value is undefined.
--
-- Generated bindings for @MPSCNNCrossChannelNormalization@.
module ObjC.MetalPerformanceShaders.MPSCNNCrossChannelNormalization
  ( MPSCNNCrossChannelNormalization
  , IsMPSCNNCrossChannelNormalization(..)
  , initWithDevice_kernelSize
  , initWithCoder_device
  , initWithDevice
  , alpha
  , setAlpha
  , beta
  , setBeta
  , delta
  , setDelta
  , kernelSize
  , alphaSelector
  , betaSelector
  , deltaSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_kernelSizeSelector
  , kernelSizeSelector
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

-- | Initialize a local response normalization filter in a channel
--
-- @device@ — The device the filter will run on
--
-- @kernelSize@ — The kernel filter size in each dimension.
--
-- Returns: A valid MPSCNNCrossChannelNormalization object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:kernelSize:@
initWithDevice_kernelSize :: IsMPSCNNCrossChannelNormalization mpscnnCrossChannelNormalization => mpscnnCrossChannelNormalization -> RawId -> CULong -> IO (Id MPSCNNCrossChannelNormalization)
initWithDevice_kernelSize mpscnnCrossChannelNormalization device kernelSize =
  sendOwnedMessage mpscnnCrossChannelNormalization initWithDevice_kernelSizeSelector device kernelSize

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
initWithCoder_device :: (IsMPSCNNCrossChannelNormalization mpscnnCrossChannelNormalization, IsNSCoder aDecoder) => mpscnnCrossChannelNormalization -> aDecoder -> RawId -> IO (Id MPSCNNCrossChannelNormalization)
initWithCoder_device mpscnnCrossChannelNormalization aDecoder device =
  sendOwnedMessage mpscnnCrossChannelNormalization initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNCrossChannelNormalization mpscnnCrossChannelNormalization => mpscnnCrossChannelNormalization -> RawId -> IO (Id MPSCNNCrossChannelNormalization)
initWithDevice mpscnnCrossChannelNormalization device =
  sendOwnedMessage mpscnnCrossChannelNormalization initWithDeviceSelector device

-- | alpha
--
-- The value of alpha.  Default is 1.0. Must be non-negative.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSCNNCrossChannelNormalization mpscnnCrossChannelNormalization => mpscnnCrossChannelNormalization -> IO CFloat
alpha mpscnnCrossChannelNormalization =
  sendMessage mpscnnCrossChannelNormalization alphaSelector

-- | alpha
--
-- The value of alpha.  Default is 1.0. Must be non-negative.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSCNNCrossChannelNormalization mpscnnCrossChannelNormalization => mpscnnCrossChannelNormalization -> CFloat -> IO ()
setAlpha mpscnnCrossChannelNormalization value =
  sendMessage mpscnnCrossChannelNormalization setAlphaSelector value

-- | beta
--
-- The value of beta.  Default is 5.0
--
-- ObjC selector: @- beta@
beta :: IsMPSCNNCrossChannelNormalization mpscnnCrossChannelNormalization => mpscnnCrossChannelNormalization -> IO CFloat
beta mpscnnCrossChannelNormalization =
  sendMessage mpscnnCrossChannelNormalization betaSelector

-- | beta
--
-- The value of beta.  Default is 5.0
--
-- ObjC selector: @- setBeta:@
setBeta :: IsMPSCNNCrossChannelNormalization mpscnnCrossChannelNormalization => mpscnnCrossChannelNormalization -> CFloat -> IO ()
setBeta mpscnnCrossChannelNormalization value =
  sendMessage mpscnnCrossChannelNormalization setBetaSelector value

-- | delta
--
-- The value of delta.  Default is 1.0
--
-- ObjC selector: @- delta@
delta :: IsMPSCNNCrossChannelNormalization mpscnnCrossChannelNormalization => mpscnnCrossChannelNormalization -> IO CFloat
delta mpscnnCrossChannelNormalization =
  sendMessage mpscnnCrossChannelNormalization deltaSelector

-- | delta
--
-- The value of delta.  Default is 1.0
--
-- ObjC selector: @- setDelta:@
setDelta :: IsMPSCNNCrossChannelNormalization mpscnnCrossChannelNormalization => mpscnnCrossChannelNormalization -> CFloat -> IO ()
setDelta mpscnnCrossChannelNormalization value =
  sendMessage mpscnnCrossChannelNormalization setDeltaSelector value

-- | kernelSize
--
-- The size of the square filter window.  Default is 5
--
-- ObjC selector: @- kernelSize@
kernelSize :: IsMPSCNNCrossChannelNormalization mpscnnCrossChannelNormalization => mpscnnCrossChannelNormalization -> IO CULong
kernelSize mpscnnCrossChannelNormalization =
  sendMessage mpscnnCrossChannelNormalization kernelSizeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelSize:@
initWithDevice_kernelSizeSelector :: Selector '[RawId, CULong] (Id MPSCNNCrossChannelNormalization)
initWithDevice_kernelSizeSelector = mkSelector "initWithDevice:kernelSize:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNCrossChannelNormalization)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNCrossChannelNormalization)
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

-- | @Selector@ for @kernelSize@
kernelSizeSelector :: Selector '[] CULong
kernelSizeSelector = mkSelector "kernelSize"

