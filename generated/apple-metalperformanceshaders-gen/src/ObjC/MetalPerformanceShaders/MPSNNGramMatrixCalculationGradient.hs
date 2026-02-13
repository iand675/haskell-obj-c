{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNGramMatrixCalculationGradient
--
-- This depends on Metal.framework
--
-- The MPSNNGramMatrixCalculationGradient defines the gradient filter for MPSNNGramMatrixCalculation.
--
-- Generated bindings for @MPSNNGramMatrixCalculationGradient@.
module ObjC.MetalPerformanceShaders.MPSNNGramMatrixCalculationGradient
  ( MPSNNGramMatrixCalculationGradient
  , IsMPSNNGramMatrixCalculationGradient(..)
  , initWithCoder_device
  , initWithDevice_alpha
  , initWithDevice
  , alpha
  , setAlpha
  , alphaSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_alphaSelector
  , setAlphaSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

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
initWithCoder_device :: (IsMPSNNGramMatrixCalculationGradient mpsnnGramMatrixCalculationGradient, IsNSCoder aDecoder) => mpsnnGramMatrixCalculationGradient -> aDecoder -> RawId -> IO (Id MPSNNGramMatrixCalculationGradient)
initWithCoder_device mpsnnGramMatrixCalculationGradient aDecoder device =
  sendOwnedMessage mpsnnGramMatrixCalculationGradient initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | Initializes a MPSNNGramMatrixCalculationGradient kernel.
--
-- @device@ — The MTLDevice on which this MPSNNGramMatrixCalculationGradient filter will be used.
--
-- @alpha@ — Scaling factor for the output. NOTE: the value for alpha is automatically adjusted by                          the MPSNNGradientState when it is provided in the encode call.
--
-- Returns: A valid MPSNNGramMatrixCalculationGradient object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:alpha:@
initWithDevice_alpha :: IsMPSNNGramMatrixCalculationGradient mpsnnGramMatrixCalculationGradient => mpsnnGramMatrixCalculationGradient -> RawId -> CFloat -> IO (Id MPSNNGramMatrixCalculationGradient)
initWithDevice_alpha mpsnnGramMatrixCalculationGradient device alpha =
  sendOwnedMessage mpsnnGramMatrixCalculationGradient initWithDevice_alphaSelector device alpha

-- | Initializes a MPSNNGramMatrixCalculationGradient kernel with scaling factor alpha = 1.0f.
--
-- @device@ — The MTLDevice on which this MPSNNGramMatrixCalculationGradient filter will be used.
--
-- Returns: A valid MPSNNGramMatrixCalculationGradient object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSNNGramMatrixCalculationGradient mpsnnGramMatrixCalculationGradient => mpsnnGramMatrixCalculationGradient -> RawId -> IO (Id MPSNNGramMatrixCalculationGradient)
initWithDevice mpsnnGramMatrixCalculationGradient device =
  sendOwnedMessage mpsnnGramMatrixCalculationGradient initWithDeviceSelector device

-- | alpha
--
-- Scaling factor for the output. Default: 1.0f. NOTE: the value for alpha is automatically adjusted by              the MPSNNGradientState when it is provided in the encode call.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSNNGramMatrixCalculationGradient mpsnnGramMatrixCalculationGradient => mpsnnGramMatrixCalculationGradient -> IO CFloat
alpha mpsnnGramMatrixCalculationGradient =
  sendMessage mpsnnGramMatrixCalculationGradient alphaSelector

-- | alpha
--
-- Scaling factor for the output. Default: 1.0f. NOTE: the value for alpha is automatically adjusted by              the MPSNNGradientState when it is provided in the encode call.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSNNGramMatrixCalculationGradient mpsnnGramMatrixCalculationGradient => mpsnnGramMatrixCalculationGradient -> CFloat -> IO ()
setAlpha mpsnnGramMatrixCalculationGradient value =
  sendMessage mpsnnGramMatrixCalculationGradient setAlphaSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNGramMatrixCalculationGradient)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:alpha:@
initWithDevice_alphaSelector :: Selector '[RawId, CFloat] (Id MPSNNGramMatrixCalculationGradient)
initWithDevice_alphaSelector = mkSelector "initWithDevice:alpha:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNGramMatrixCalculationGradient)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector '[] CFloat
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @setAlpha:@
setAlphaSelector :: Selector '[CFloat] ()
setAlphaSelector = mkSelector "setAlpha:"

