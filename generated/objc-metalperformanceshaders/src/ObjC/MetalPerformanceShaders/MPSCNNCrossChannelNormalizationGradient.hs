{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNCrossChannelNormalizationGradient
--
-- This depends on Metal.framework
--
-- Specifies the normalization gradient filter across feature channels.               This normalization filter applies the filter to a local region across nearby feature channels,              but with no spatial extent (i.e., they have shape kernelSize x 1 x 1).              The normalized output is given by:                  Y(i,j,k) = X(i,j,k) / L(i,j,k)^beta,              where the normalizing factor is:                  L(i,j,k) = delta + alpha/N * (sum_{q in Q(k)} X(i,j,q)^2, where              N is the kernel size. The window Q(k) itself is defined as:                  Q(k) = [max(0, k-floor(N/2)), min(D-1, k+floor((N-1)/2)], where
--
-- k is the feature channel index (running from 0 to D-1) and              D is the number of feature channels, and alpha, beta and delta are paremeters.              It is the end-users responsibility to ensure that the combination of the              parameters delta and alpha does not result in a situation where the denominator              becomes zero - in such situations the resulting pixel-value is undefined.
--
-- OutputGradient:                  dZ/dX(i,j,k) = dZ/dY(i,j,k) * (L(i,j,k)^-beta) - 2 * alpha * beta * X(i,j,k) * ( sum_{r in R(k)} dZ/dY(i,j,r) * X(i,j,r) * (L(i,j,r) ^ (-beta-1)) )              N is the kernel size. The window L(i) and K(j) itself is defined as:                  R(k) = [max(0, k-floor((N-1)/2)), min(D-1, k+floor(N/2)]
--
-- For correct gradient computation all parameters must be the same as the original normalization filter.
--
-- Generated bindings for @MPSCNNCrossChannelNormalizationGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNCrossChannelNormalizationGradient
  ( MPSCNNCrossChannelNormalizationGradient
  , IsMPSCNNCrossChannelNormalizationGradient(..)
  , initWithDevice_kernelSize
  , initWithCoder_device
  , alpha
  , setAlpha
  , beta
  , setBeta
  , delta
  , setDelta
  , kernelSize
  , initWithDevice_kernelSizeSelector
  , initWithCoder_deviceSelector
  , alphaSelector
  , setAlphaSelector
  , betaSelector
  , setBetaSelector
  , deltaSelector
  , setDeltaSelector
  , kernelSizeSelector


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

-- | Initialize a cross channel normalization gradient filter
--
-- @device@ — The device the filter will run on
--
-- @kernelSize@ — The kernel filter size in each dimension.
--
-- Returns: A valid MPSCNNCrossChannelNormalization object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:kernelSize:@
initWithDevice_kernelSize :: IsMPSCNNCrossChannelNormalizationGradient mpscnnCrossChannelNormalizationGradient => mpscnnCrossChannelNormalizationGradient -> RawId -> CULong -> IO (Id MPSCNNCrossChannelNormalizationGradient)
initWithDevice_kernelSize mpscnnCrossChannelNormalizationGradient  device kernelSize =
  sendMsg mpscnnCrossChannelNormalizationGradient (mkSelector "initWithDevice:kernelSize:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral kernelSize)] >>= ownedObject . castPtr

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
initWithCoder_device :: (IsMPSCNNCrossChannelNormalizationGradient mpscnnCrossChannelNormalizationGradient, IsNSCoder aDecoder) => mpscnnCrossChannelNormalizationGradient -> aDecoder -> RawId -> IO (Id MPSCNNCrossChannelNormalizationGradient)
initWithCoder_device mpscnnCrossChannelNormalizationGradient  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpscnnCrossChannelNormalizationGradient (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | alpha
--
-- The value of alpha.  Default is 1.0. Must be non-negative.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSCNNCrossChannelNormalizationGradient mpscnnCrossChannelNormalizationGradient => mpscnnCrossChannelNormalizationGradient -> IO CFloat
alpha mpscnnCrossChannelNormalizationGradient  =
  sendMsg mpscnnCrossChannelNormalizationGradient (mkSelector "alpha") retCFloat []

-- | alpha
--
-- The value of alpha.  Default is 1.0. Must be non-negative.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSCNNCrossChannelNormalizationGradient mpscnnCrossChannelNormalizationGradient => mpscnnCrossChannelNormalizationGradient -> CFloat -> IO ()
setAlpha mpscnnCrossChannelNormalizationGradient  value =
  sendMsg mpscnnCrossChannelNormalizationGradient (mkSelector "setAlpha:") retVoid [argCFloat (fromIntegral value)]

-- | beta
--
-- The value of beta.  Default is 5.0
--
-- ObjC selector: @- beta@
beta :: IsMPSCNNCrossChannelNormalizationGradient mpscnnCrossChannelNormalizationGradient => mpscnnCrossChannelNormalizationGradient -> IO CFloat
beta mpscnnCrossChannelNormalizationGradient  =
  sendMsg mpscnnCrossChannelNormalizationGradient (mkSelector "beta") retCFloat []

-- | beta
--
-- The value of beta.  Default is 5.0
--
-- ObjC selector: @- setBeta:@
setBeta :: IsMPSCNNCrossChannelNormalizationGradient mpscnnCrossChannelNormalizationGradient => mpscnnCrossChannelNormalizationGradient -> CFloat -> IO ()
setBeta mpscnnCrossChannelNormalizationGradient  value =
  sendMsg mpscnnCrossChannelNormalizationGradient (mkSelector "setBeta:") retVoid [argCFloat (fromIntegral value)]

-- | delta
--
-- The value of delta.  Default is 1.0
--
-- ObjC selector: @- delta@
delta :: IsMPSCNNCrossChannelNormalizationGradient mpscnnCrossChannelNormalizationGradient => mpscnnCrossChannelNormalizationGradient -> IO CFloat
delta mpscnnCrossChannelNormalizationGradient  =
  sendMsg mpscnnCrossChannelNormalizationGradient (mkSelector "delta") retCFloat []

-- | delta
--
-- The value of delta.  Default is 1.0
--
-- ObjC selector: @- setDelta:@
setDelta :: IsMPSCNNCrossChannelNormalizationGradient mpscnnCrossChannelNormalizationGradient => mpscnnCrossChannelNormalizationGradient -> CFloat -> IO ()
setDelta mpscnnCrossChannelNormalizationGradient  value =
  sendMsg mpscnnCrossChannelNormalizationGradient (mkSelector "setDelta:") retVoid [argCFloat (fromIntegral value)]

-- | kernelSize
--
-- The size of the square filter window.  Default is 5
--
-- ObjC selector: @- kernelSize@
kernelSize :: IsMPSCNNCrossChannelNormalizationGradient mpscnnCrossChannelNormalizationGradient => mpscnnCrossChannelNormalizationGradient -> IO CULong
kernelSize mpscnnCrossChannelNormalizationGradient  =
  sendMsg mpscnnCrossChannelNormalizationGradient (mkSelector "kernelSize") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelSize:@
initWithDevice_kernelSizeSelector :: Selector
initWithDevice_kernelSizeSelector = mkSelector "initWithDevice:kernelSize:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

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

-- | @Selector@ for @kernelSize@
kernelSizeSelector :: Selector
kernelSizeSelector = mkSelector "kernelSize"

