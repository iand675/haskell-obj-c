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
  , initWithDevice_kernelWidth_kernelHeightSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , alphaSelector
  , setAlphaSelector
  , betaSelector
  , setBetaSelector
  , deltaSelector
  , setDeltaSelector
  , p0Selector
  , setP0Selector
  , pmSelector
  , setPmSelector
  , psSelector
  , setPsSelector


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
initWithDevice_kernelWidth_kernelHeight mpscnnLocalContrastNormalization  device kernelWidth kernelHeight =
  sendMsg mpscnnLocalContrastNormalization (mkSelector "initWithDevice:kernelWidth:kernelHeight:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight)] >>= ownedObject . castPtr

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
initWithCoder_device mpscnnLocalContrastNormalization  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpscnnLocalContrastNormalization (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> RawId -> IO (Id MPSCNNLocalContrastNormalization)
initWithDevice mpscnnLocalContrastNormalization  device =
  sendMsg mpscnnLocalContrastNormalization (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | alpha
--
-- The value of alpha.  Default is 0.0
--
-- The default value 0.0 is not recommended and is              preserved for backwards compatibility. With alpha 0,              it performs a local mean subtraction. The              MPSCNNLocalContrastNormalizationNode used with              the MPSNNGraph uses 1.0 as a default.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> IO CFloat
alpha mpscnnLocalContrastNormalization  =
  sendMsg mpscnnLocalContrastNormalization (mkSelector "alpha") retCFloat []

-- | alpha
--
-- The value of alpha.  Default is 0.0
--
-- The default value 0.0 is not recommended and is              preserved for backwards compatibility. With alpha 0,              it performs a local mean subtraction. The              MPSCNNLocalContrastNormalizationNode used with              the MPSNNGraph uses 1.0 as a default.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> CFloat -> IO ()
setAlpha mpscnnLocalContrastNormalization  value =
  sendMsg mpscnnLocalContrastNormalization (mkSelector "setAlpha:") retVoid [argCFloat (fromIntegral value)]

-- | beta
--
-- The value of beta.  Default is 0.5
--
-- ObjC selector: @- beta@
beta :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> IO CFloat
beta mpscnnLocalContrastNormalization  =
  sendMsg mpscnnLocalContrastNormalization (mkSelector "beta") retCFloat []

-- | beta
--
-- The value of beta.  Default is 0.5
--
-- ObjC selector: @- setBeta:@
setBeta :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> CFloat -> IO ()
setBeta mpscnnLocalContrastNormalization  value =
  sendMsg mpscnnLocalContrastNormalization (mkSelector "setBeta:") retVoid [argCFloat (fromIntegral value)]

-- | delta
--
-- The value of delta.  Default is 1/1024
--
-- ObjC selector: @- delta@
delta :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> IO CFloat
delta mpscnnLocalContrastNormalization  =
  sendMsg mpscnnLocalContrastNormalization (mkSelector "delta") retCFloat []

-- | delta
--
-- The value of delta.  Default is 1/1024
--
-- ObjC selector: @- setDelta:@
setDelta :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> CFloat -> IO ()
setDelta mpscnnLocalContrastNormalization  value =
  sendMsg mpscnnLocalContrastNormalization (mkSelector "setDelta:") retVoid [argCFloat (fromIntegral value)]

-- | p0
--
-- The value of p0.  Default is 1.0
--
-- ObjC selector: @- p0@
p0 :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> IO CFloat
p0 mpscnnLocalContrastNormalization  =
  sendMsg mpscnnLocalContrastNormalization (mkSelector "p0") retCFloat []

-- | p0
--
-- The value of p0.  Default is 1.0
--
-- ObjC selector: @- setP0:@
setP0 :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> CFloat -> IO ()
setP0 mpscnnLocalContrastNormalization  value =
  sendMsg mpscnnLocalContrastNormalization (mkSelector "setP0:") retVoid [argCFloat (fromIntegral value)]

-- | pm
--
-- The value of pm.  Default is 0.0
--
-- ObjC selector: @- pm@
pm :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> IO CFloat
pm mpscnnLocalContrastNormalization  =
  sendMsg mpscnnLocalContrastNormalization (mkSelector "pm") retCFloat []

-- | pm
--
-- The value of pm.  Default is 0.0
--
-- ObjC selector: @- setPm:@
setPm :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> CFloat -> IO ()
setPm mpscnnLocalContrastNormalization  value =
  sendMsg mpscnnLocalContrastNormalization (mkSelector "setPm:") retVoid [argCFloat (fromIntegral value)]

-- | ps
--
-- The value of ps.  Default is 1.0
--
-- ObjC selector: @- ps@
ps :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> IO CFloat
ps mpscnnLocalContrastNormalization  =
  sendMsg mpscnnLocalContrastNormalization (mkSelector "ps") retCFloat []

-- | ps
--
-- The value of ps.  Default is 1.0
--
-- ObjC selector: @- setPs:@
setPs :: IsMPSCNNLocalContrastNormalization mpscnnLocalContrastNormalization => mpscnnLocalContrastNormalization -> CFloat -> IO ()
setPs mpscnnLocalContrastNormalization  value =
  sendMsg mpscnnLocalContrastNormalization (mkSelector "setPs:") retVoid [argCFloat (fromIntegral value)]

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

-- | @Selector@ for @p0@
p0Selector :: Selector
p0Selector = mkSelector "p0"

-- | @Selector@ for @setP0:@
setP0Selector :: Selector
setP0Selector = mkSelector "setP0:"

-- | @Selector@ for @pm@
pmSelector :: Selector
pmSelector = mkSelector "pm"

-- | @Selector@ for @setPm:@
setPmSelector :: Selector
setPmSelector = mkSelector "setPm:"

-- | @Selector@ for @ps@
psSelector :: Selector
psSelector = mkSelector "ps"

-- | @Selector@ for @setPs:@
setPsSelector :: Selector
setPsSelector = mkSelector "setPs:"

