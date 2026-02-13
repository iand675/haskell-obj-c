{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNSpatialNormalizationGradient
--
-- This depends on Metal.framework
--
-- Specifies the spatial normalization gradient filter.              The spatial normalization for a feature channel applies the filter over local regions which extend              spatially, but are in separate feature channels (i.e., they have shape 1 x kernelWidth x kernelHeight).              For each feature channel, the function computes the sum of squares of X inside each rectangle, N2(i,j).              It then divides each element of X as follows:                  Y(i,j) = X(i,j) / (delta + alpha/(kw*kh) * N2(i,j))^beta,              where kw and kh are the kernelWidth and the kernelHeight.              It is the end-users responsibility to ensure that the combination of the              parameters delta and alpha does not result in a situation where the denominator              becomes zero - in such situations the resulting pixel-value is undefined.
--
-- T(i,j) = (delta + alpha/(kw*kh) * N2(i,j))              N      = kw * kh
--
-- OutputGradient:                  dZ/dX(i,j) =  T(i,j)^(-beta) * ( dZ/dY(i,j) - (2*alpha*beta*X(i,j)/T(i,j)) * (sum_{l,k in L(i),K(j)} dZ/dY(l,k)*X(l,k)) )              N is the kernel size. The window R(k) itself is defined as:                  L(i) = [i-floor((kw-1)/2), i+floor(kw/2]                  K(j) = [j-floor((kh-1)/2), j+floor(kh/2]
--
-- For correct gradient computation all parameters must be the same as the original normalization filter.
--
-- Generated bindings for @MPSCNNSpatialNormalizationGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNSpatialNormalizationGradient
  ( MPSCNNSpatialNormalizationGradient
  , IsMPSCNNSpatialNormalizationGradient(..)
  , initWithDevice_kernelWidth_kernelHeight
  , initWithCoder_device
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
initWithDevice_kernelWidth_kernelHeight :: IsMPSCNNSpatialNormalizationGradient mpscnnSpatialNormalizationGradient => mpscnnSpatialNormalizationGradient -> RawId -> CULong -> CULong -> IO (Id MPSCNNSpatialNormalizationGradient)
initWithDevice_kernelWidth_kernelHeight mpscnnSpatialNormalizationGradient device kernelWidth kernelHeight =
  sendOwnedMessage mpscnnSpatialNormalizationGradient initWithDevice_kernelWidth_kernelHeightSelector device kernelWidth kernelHeight

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
initWithCoder_device :: (IsMPSCNNSpatialNormalizationGradient mpscnnSpatialNormalizationGradient, IsNSCoder aDecoder) => mpscnnSpatialNormalizationGradient -> aDecoder -> RawId -> IO (Id MPSCNNSpatialNormalizationGradient)
initWithCoder_device mpscnnSpatialNormalizationGradient aDecoder device =
  sendOwnedMessage mpscnnSpatialNormalizationGradient initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | alpha
--
-- The value of alpha.  Default is 1.0. Must be non-negative.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSCNNSpatialNormalizationGradient mpscnnSpatialNormalizationGradient => mpscnnSpatialNormalizationGradient -> IO CFloat
alpha mpscnnSpatialNormalizationGradient =
  sendMessage mpscnnSpatialNormalizationGradient alphaSelector

-- | alpha
--
-- The value of alpha.  Default is 1.0. Must be non-negative.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSCNNSpatialNormalizationGradient mpscnnSpatialNormalizationGradient => mpscnnSpatialNormalizationGradient -> CFloat -> IO ()
setAlpha mpscnnSpatialNormalizationGradient value =
  sendMessage mpscnnSpatialNormalizationGradient setAlphaSelector value

-- | beta
--
-- The value of beta.  Default is 5.0
--
-- ObjC selector: @- beta@
beta :: IsMPSCNNSpatialNormalizationGradient mpscnnSpatialNormalizationGradient => mpscnnSpatialNormalizationGradient -> IO CFloat
beta mpscnnSpatialNormalizationGradient =
  sendMessage mpscnnSpatialNormalizationGradient betaSelector

-- | beta
--
-- The value of beta.  Default is 5.0
--
-- ObjC selector: @- setBeta:@
setBeta :: IsMPSCNNSpatialNormalizationGradient mpscnnSpatialNormalizationGradient => mpscnnSpatialNormalizationGradient -> CFloat -> IO ()
setBeta mpscnnSpatialNormalizationGradient value =
  sendMessage mpscnnSpatialNormalizationGradient setBetaSelector value

-- | delta
--
-- The value of delta.  Default is 1.0
--
-- ObjC selector: @- delta@
delta :: IsMPSCNNSpatialNormalizationGradient mpscnnSpatialNormalizationGradient => mpscnnSpatialNormalizationGradient -> IO CFloat
delta mpscnnSpatialNormalizationGradient =
  sendMessage mpscnnSpatialNormalizationGradient deltaSelector

-- | delta
--
-- The value of delta.  Default is 1.0
--
-- ObjC selector: @- setDelta:@
setDelta :: IsMPSCNNSpatialNormalizationGradient mpscnnSpatialNormalizationGradient => mpscnnSpatialNormalizationGradient -> CFloat -> IO ()
setDelta mpscnnSpatialNormalizationGradient value =
  sendMessage mpscnnSpatialNormalizationGradient setDeltaSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:@
initWithDevice_kernelWidth_kernelHeightSelector :: Selector '[RawId, CULong, CULong] (Id MPSCNNSpatialNormalizationGradient)
initWithDevice_kernelWidth_kernelHeightSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNSpatialNormalizationGradient)
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

