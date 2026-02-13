{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNLocalContrastNormalization
--
-- This depends on Metal.framework
--
-- Specifies the local contrast normalization filter.              The local contrast normalization is quite similar to spatial normalization              (see MPSCNNSpatialNormalization) in that it applies the filter over local regions which extend              spatially, but are in separate feature channels (i.e., they have shape 1 x kernelWidth x kernelHeight),              but instead of dividing by the local "energy" of the feature, the denominator uses the local variance              of the feature - effectively the mean value of the feature is subtracted from the signal.              For each feature channel, the function computes the variance VAR(i,j) and              mean M(i,j) of X(i,j) inside each rectangle around the spatial point (i,j).
--
-- Then the result is computed for each element of X as follows:
--
-- Y(i,j) = pm + ps * ( X(i,j) - p0 * M(i,j)) / (delta + alpha * VAR(i,j))^beta,
--
-- where kw and kh are the kernelWidth and the kernelHeight and pm, ps and p0 are parameters that              can be used to offset and scale the result in various ways. For example setting              pm=0, ps=1, p0=1, delta=0, alpha=1.0 and beta=0.5 scales input data so that the result has              unit variance and zero mean, provided that input variance is positive.              It is the end-users responsibility to ensure that the combination of the              parameters delta and alpha does not result in a situation where the denominator              becomes zero - in such situations the resulting pixel-value is undefined. A good way to guard              against tiny variances is to regulate the expression with a small value for delta, for example              delta = 1/1024 = 0.0009765625.
--
-- Generated bindings for @MPSCNNLocalContrastNormalization@.
module ObjC.MetalPerformanceShaders.MPSCNNLocalContrastNormalization
  ( MPSCNNLocalContrastNormalization
  , IsMPSCNNLocalContrastNormalization(..)
  , initWithDevice_kernelWidth_kernelHeight
  , initWithCoder_device
  , initWithDevice
  , alpha
  , setAlpha
  , beta
  , setBeta
  , delta
  , setDelta
  , p0
  , setP0
  , pm
  , setPm
  , ps
  , setPs
  , alphaSelector
  , betaSelector
  , deltaSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_kernelWidth_kernelHeightSelector
  , p0Selector
  , pmSelector
  , psSelector
  , setAlphaSelector
  , setBetaSelector
  , setDeltaSelector
  , setP0Selector
  , setPmSelector
  , setPsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize a local contrast normalization filter
--
-- @device@ — The device the filter will run on
--
-- @kernelWidth@ — The width of the kernel
--
-- @kernelHeight@ — The height of the kernel
--
-- Returns: A valid MPSCNNLocalContrastNormalization object or nil, if failure.
--
-- NOTE:  For now, kernelWidth must be equal to kernelHeight
--
-- ObjC selector: @- initWithDevice:kernelWidth:kernelHeight:@
initWithDevice_kernelWidth_kernelHeight :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> RawId -> CULong -> CULong -> IO (Id MPSCNNLocalContrastNormalization)
initWithDevice_kernelWidth_kernelHeight mpscnnLocalContrastNormalization device kernelWidth kernelHeight =
  sendOwnedMessage mpscnnLocalContrastNormalization initWithDevice_kernelWidth_kernelHeightSelector device kernelWidth kernelHeight

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
initWithCoder_device :: (IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization, IsNSCoder aDecoder) => mpscnnLocalContrastNormalization -> aDecoder -> RawId -> IO (Id MPSCNNLocalContrastNormalization)
initWithCoder_device mpscnnLocalContrastNormalization aDecoder device =
  sendOwnedMessage mpscnnLocalContrastNormalization initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> RawId -> IO (Id MPSCNNLocalContrastNormalization)
initWithDevice mpscnnLocalContrastNormalization device =
  sendOwnedMessage mpscnnLocalContrastNormalization initWithDeviceSelector device

-- | alpha
--
-- The value of alpha.  Default is 0.0
--
-- The default value 0.0 is not recommended and is              preserved for backwards compatibility. With alpha 0,              it performs a local mean subtraction. The              MPSCNNLocalContrastNormalizationNode used with              the MPSNNGraph uses 1.0 as a default.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> IO CFloat
alpha mpscnnLocalContrastNormalization =
  sendMessage mpscnnLocalContrastNormalization alphaSelector

-- | alpha
--
-- The value of alpha.  Default is 0.0
--
-- The default value 0.0 is not recommended and is              preserved for backwards compatibility. With alpha 0,              it performs a local mean subtraction. The              MPSCNNLocalContrastNormalizationNode used with              the MPSNNGraph uses 1.0 as a default.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> CFloat -> IO ()
setAlpha mpscnnLocalContrastNormalization value =
  sendMessage mpscnnLocalContrastNormalization setAlphaSelector value

-- | beta
--
-- The value of beta.  Default is 0.5
--
-- ObjC selector: @- beta@
beta :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> IO CFloat
beta mpscnnLocalContrastNormalization =
  sendMessage mpscnnLocalContrastNormalization betaSelector

-- | beta
--
-- The value of beta.  Default is 0.5
--
-- ObjC selector: @- setBeta:@
setBeta :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> CFloat -> IO ()
setBeta mpscnnLocalContrastNormalization value =
  sendMessage mpscnnLocalContrastNormalization setBetaSelector value

-- | delta
--
-- The value of delta.  Default is 1/1024
--
-- ObjC selector: @- delta@
delta :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> IO CFloat
delta mpscnnLocalContrastNormalization =
  sendMessage mpscnnLocalContrastNormalization deltaSelector

-- | delta
--
-- The value of delta.  Default is 1/1024
--
-- ObjC selector: @- setDelta:@
setDelta :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> CFloat -> IO ()
setDelta mpscnnLocalContrastNormalization value =
  sendMessage mpscnnLocalContrastNormalization setDeltaSelector value

-- | p0
--
-- The value of p0.  Default is 1.0
--
-- ObjC selector: @- p0@
p0 :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> IO CFloat
p0 mpscnnLocalContrastNormalization =
  sendMessage mpscnnLocalContrastNormalization p0Selector

-- | p0
--
-- The value of p0.  Default is 1.0
--
-- ObjC selector: @- setP0:@
setP0 :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> CFloat -> IO ()
setP0 mpscnnLocalContrastNormalization value =
  sendMessage mpscnnLocalContrastNormalization setP0Selector value

-- | pm
--
-- The value of pm.  Default is 0.0
--
-- ObjC selector: @- pm@
pm :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> IO CFloat
pm mpscnnLocalContrastNormalization =
  sendMessage mpscnnLocalContrastNormalization pmSelector

-- | pm
--
-- The value of pm.  Default is 0.0
--
-- ObjC selector: @- setPm:@
setPm :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> CFloat -> IO ()
setPm mpscnnLocalContrastNormalization value =
  sendMessage mpscnnLocalContrastNormalization setPmSelector value

-- | ps
--
-- The value of ps.  Default is 1.0
--
-- ObjC selector: @- ps@
ps :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> IO CFloat
ps mpscnnLocalContrastNormalization =
  sendMessage mpscnnLocalContrastNormalization psSelector

-- | ps
--
-- The value of ps.  Default is 1.0
--
-- ObjC selector: @- setPs:@
setPs :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> CFloat -> IO ()
setPs mpscnnLocalContrastNormalization value =
  sendMessage mpscnnLocalContrastNormalization setPsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:@
initWithDevice_kernelWidth_kernelHeightSelector :: Selector '[RawId, CULong, CULong] (Id MPSCNNLocalContrastNormalization)
initWithDevice_kernelWidth_kernelHeightSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNLocalContrastNormalization)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNLocalContrastNormalization)
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

-- | @Selector@ for @p0@
p0Selector :: Selector '[] CFloat
p0Selector = mkSelector "p0"

-- | @Selector@ for @setP0:@
setP0Selector :: Selector '[CFloat] ()
setP0Selector = mkSelector "setP0:"

-- | @Selector@ for @pm@
pmSelector :: Selector '[] CFloat
pmSelector = mkSelector "pm"

-- | @Selector@ for @setPm:@
setPmSelector :: Selector '[CFloat] ()
setPmSelector = mkSelector "setPm:"

-- | @Selector@ for @ps@
psSelector :: Selector '[] CFloat
psSelector = mkSelector "ps"

-- | @Selector@ for @setPs:@
setPsSelector :: Selector '[CFloat] ()
setPsSelector = mkSelector "setPs:"

