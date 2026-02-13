{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNDropoutGradient
--
-- This depends on Metal.framework
--
-- This filter is the backward filter for the MPSCNNDropout forward filter.              It requires the mask data, along with all the associated parameters used              to generate the mask, from the forward pass. The mask is associated with              a MPSCNNDropoutGradientState object.
--
-- In this kernel, use the secondaryOffset to apply an offset to the mask data.
--
-- Generated bindings for @MPSCNNDropoutGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNDropoutGradient
  ( MPSCNNDropoutGradient
  , IsMPSCNNDropoutGradient(..)
  , initWithDevice
  , initWithCoder_device
  , keepProbability
  , seed
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , keepProbabilitySelector
  , seedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNDropoutGradient mpscnnDropoutGradient => mpscnnDropoutGradient -> RawId -> IO (Id MPSCNNDropoutGradient)
initWithDevice mpscnnDropoutGradient device =
  sendOwnedMessage mpscnnDropoutGradient initWithDeviceSelector device

-- | <NSSecureCoding> support
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNDropoutGradient mpscnnDropoutGradient, IsNSCoder aDecoder) => mpscnnDropoutGradient -> aDecoder -> RawId -> IO (Id MPSCNNDropoutGradient)
initWithCoder_device mpscnnDropoutGradient aDecoder device =
  sendOwnedMessage mpscnnDropoutGradient initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | keepProbability
--
-- The probability that each element in the input is kept.              The valid range is (0.0f, 1.0f).
--
-- ObjC selector: @- keepProbability@
keepProbability :: IsMPSCNNDropoutGradient mpscnnDropoutGradient => mpscnnDropoutGradient -> IO CFloat
keepProbability mpscnnDropoutGradient =
  sendMessage mpscnnDropoutGradient keepProbabilitySelector

-- | seed
--
-- The seed used to generate random numbers.
--
-- ObjC selector: @- seed@
seed :: IsMPSCNNDropoutGradient mpscnnDropoutGradient => mpscnnDropoutGradient -> IO CULong
seed mpscnnDropoutGradient =
  sendMessage mpscnnDropoutGradient seedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNDropoutGradient)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNDropoutGradient)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @keepProbability@
keepProbabilitySelector :: Selector '[] CFloat
keepProbabilitySelector = mkSelector "keepProbability"

-- | @Selector@ for @seed@
seedSelector :: Selector '[] CULong
seedSelector = mkSelector "seed"

