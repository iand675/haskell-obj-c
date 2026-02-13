{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNLocalContrastNormalizationGradient
--
-- This depends on Metal.framework
--
-- Specifies the local contrast normalization gradient filter.              The local contrast normalization is quite similar to spatial normalization              (see MPSCNNSpatialNormalization) in that it applies the filter over local regions which extend              spatially, but are in separate feature channels (i.e., they have shape 1 x kernelWidth x kernelHeight),              but instead of dividing by the local "energy" of the feature, the denominator uses the local variance              of the feature - effectively the mean value of the feature is subtracted from the signal.              For each feature channel, the function computes the variance VAR(i,j) and              mean M(i,j) of X(i,j) inside each rectangle around the spatial point (i,j).
--
-- Then the result is computed for each element of X as follows:
--
-- Y(i,j) = pm + ps * ( X(i,j) - p0 * M(i,j)) / (delta + alpha * VAR(i,j))^beta,
--
-- where kw and kh are the kernelWidth and the kernelHeight and pm, ps and p0 are parameters that              can be used to offset and scale the result in various ways. For example setting              pm=0, ps=1, p0=1, delta=0, alpha=1.0 and beta=0.5 scales input data so that the result has              unit variance and zero mean, provided that input variance is positive.              It is the end-users responsibility to ensure that the combination of the              parameters delta and alpha does not result in a situation where the denominator              becomes zero - in such situations the resulting pixel-value is undefined. A good way to guard              against tiny variances is to regulate the expression with a small value for delta, for example              delta = 1/1024 = 0.0009765625.
--
-- T(i,j) = (delta + alpha * VAR(i,j))              N      = kw * kh
--
-- OutputGradient:                  dZ/dX(i,j) =  ps * T(i,j)^(-beta) * ( dZ/dY(i,j) - (sum_{l,k in L(i),K(j)} dZ/dY(l,k) * (((p0/N) + (2*alpha*beta/N)*(X(k,l)-1)*(X(i,j)-M(i,j)*p0)/T(i,j)))) )              N is the kernel size. The window L(i) and K(j) itself is defined as:                  L(i) = [i-floor((kw-1)/2), i+floor(kw/2]                  K(j) = [j-floor((kh-1)/2), j+floor(kh/2]
--
-- For correct gradient computation all parameters must be the same as the original normalization filter.
--
-- Generated bindings for @MPSCNNLocalContrastNormalizationGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNLocalContrastNormalizationGradient
  ( MPSCNNLocalContrastNormalizationGradient
  , IsMPSCNNLocalContrastNormalizationGradient(..)
  , initWithDevice_kernelWidth_kernelHeight
  , initWithCoder_device
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
initWithDevice_kernelWidth_kernelHeight :: IsMPSCNNLocalContrastNormalizationGradient mpscnnLocalContrastNormalizationGradient => mpscnnLocalContrastNormalizationGradient -> RawId -> CULong -> CULong -> IO (Id MPSCNNLocalContrastNormalizationGradient)
initWithDevice_kernelWidth_kernelHeight mpscnnLocalContrastNormalizationGradient device kernelWidth kernelHeight =
  sendOwnedMessage mpscnnLocalContrastNormalizationGradient initWithDevice_kernelWidth_kernelHeightSelector device kernelWidth kernelHeight

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
initWithCoder_device :: (IsMPSCNNLocalContrastNormalizationGradient mpscnnLocalContrastNormalizationGradient, IsNSCoder aDecoder) => mpscnnLocalContrastNormalizationGradient -> aDecoder -> RawId -> IO (Id MPSCNNLocalContrastNormalizationGradient)
initWithCoder_device mpscnnLocalContrastNormalizationGradient aDecoder device =
  sendOwnedMessage mpscnnLocalContrastNormalizationGradient initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | alpha
--
-- The value of alpha.  Default is 0.0
--
-- The default value 0.0 is not recommended and is              preserved for backwards compatibility. With alpha 0,              it performs a local mean subtraction. The              MPSCNNLocalContrastNormalizationNode used with              the MPSNNGraph uses 1.0 as a default.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSCNNLocalContrastNormalizationGradient mpscnnLocalContrastNormalizationGradient => mpscnnLocalContrastNormalizationGradient -> IO CFloat
alpha mpscnnLocalContrastNormalizationGradient =
  sendMessage mpscnnLocalContrastNormalizationGradient alphaSelector

-- | alpha
--
-- The value of alpha.  Default is 0.0
--
-- The default value 0.0 is not recommended and is              preserved for backwards compatibility. With alpha 0,              it performs a local mean subtraction. The              MPSCNNLocalContrastNormalizationNode used with              the MPSNNGraph uses 1.0 as a default.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSCNNLocalContrastNormalizationGradient mpscnnLocalContrastNormalizationGradient => mpscnnLocalContrastNormalizationGradient -> CFloat -> IO ()
setAlpha mpscnnLocalContrastNormalizationGradient value =
  sendMessage mpscnnLocalContrastNormalizationGradient setAlphaSelector value

-- | beta
--
-- The value of beta.  Default is 0.5
--
-- ObjC selector: @- beta@
beta :: IsMPSCNNLocalContrastNormalizationGradient mpscnnLocalContrastNormalizationGradient => mpscnnLocalContrastNormalizationGradient -> IO CFloat
beta mpscnnLocalContrastNormalizationGradient =
  sendMessage mpscnnLocalContrastNormalizationGradient betaSelector

-- | beta
--
-- The value of beta.  Default is 0.5
--
-- ObjC selector: @- setBeta:@
setBeta :: IsMPSCNNLocalContrastNormalizationGradient mpscnnLocalContrastNormalizationGradient => mpscnnLocalContrastNormalizationGradient -> CFloat -> IO ()
setBeta mpscnnLocalContrastNormalizationGradient value =
  sendMessage mpscnnLocalContrastNormalizationGradient setBetaSelector value

-- | delta
--
-- The value of delta.  Default is 1/1024
--
-- ObjC selector: @- delta@
delta :: IsMPSCNNLocalContrastNormalizationGradient mpscnnLocalContrastNormalizationGradient => mpscnnLocalContrastNormalizationGradient -> IO CFloat
delta mpscnnLocalContrastNormalizationGradient =
  sendMessage mpscnnLocalContrastNormalizationGradient deltaSelector

-- | delta
--
-- The value of delta.  Default is 1/1024
--
-- ObjC selector: @- setDelta:@
setDelta :: IsMPSCNNLocalContrastNormalizationGradient mpscnnLocalContrastNormalizationGradient => mpscnnLocalContrastNormalizationGradient -> CFloat -> IO ()
setDelta mpscnnLocalContrastNormalizationGradient value =
  sendMessage mpscnnLocalContrastNormalizationGradient setDeltaSelector value

-- | p0
--
-- The value of p0.  Default is 1.0
--
-- ObjC selector: @- p0@
p0 :: IsMPSCNNLocalContrastNormalizationGradient mpscnnLocalContrastNormalizationGradient => mpscnnLocalContrastNormalizationGradient -> IO CFloat
p0 mpscnnLocalContrastNormalizationGradient =
  sendMessage mpscnnLocalContrastNormalizationGradient p0Selector

-- | p0
--
-- The value of p0.  Default is 1.0
--
-- ObjC selector: @- setP0:@
setP0 :: IsMPSCNNLocalContrastNormalizationGradient mpscnnLocalContrastNormalizationGradient => mpscnnLocalContrastNormalizationGradient -> CFloat -> IO ()
setP0 mpscnnLocalContrastNormalizationGradient value =
  sendMessage mpscnnLocalContrastNormalizationGradient setP0Selector value

-- | pm
--
-- The value of pm.  Default is 0.0
--
-- ObjC selector: @- pm@
pm :: IsMPSCNNLocalContrastNormalizationGradient mpscnnLocalContrastNormalizationGradient => mpscnnLocalContrastNormalizationGradient -> IO CFloat
pm mpscnnLocalContrastNormalizationGradient =
  sendMessage mpscnnLocalContrastNormalizationGradient pmSelector

-- | pm
--
-- The value of pm.  Default is 0.0
--
-- ObjC selector: @- setPm:@
setPm :: IsMPSCNNLocalContrastNormalizationGradient mpscnnLocalContrastNormalizationGradient => mpscnnLocalContrastNormalizationGradient -> CFloat -> IO ()
setPm mpscnnLocalContrastNormalizationGradient value =
  sendMessage mpscnnLocalContrastNormalizationGradient setPmSelector value

-- | ps
--
-- The value of ps.  Default is 1.0
--
-- ObjC selector: @- ps@
ps :: IsMPSCNNLocalContrastNormalizationGradient mpscnnLocalContrastNormalizationGradient => mpscnnLocalContrastNormalizationGradient -> IO CFloat
ps mpscnnLocalContrastNormalizationGradient =
  sendMessage mpscnnLocalContrastNormalizationGradient psSelector

-- | ps
--
-- The value of ps.  Default is 1.0
--
-- ObjC selector: @- setPs:@
setPs :: IsMPSCNNLocalContrastNormalizationGradient mpscnnLocalContrastNormalizationGradient => mpscnnLocalContrastNormalizationGradient -> CFloat -> IO ()
setPs mpscnnLocalContrastNormalizationGradient value =
  sendMessage mpscnnLocalContrastNormalizationGradient setPsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:@
initWithDevice_kernelWidth_kernelHeightSelector :: Selector '[RawId, CULong, CULong] (Id MPSCNNLocalContrastNormalizationGradient)
initWithDevice_kernelWidth_kernelHeightSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNLocalContrastNormalizationGradient)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

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

