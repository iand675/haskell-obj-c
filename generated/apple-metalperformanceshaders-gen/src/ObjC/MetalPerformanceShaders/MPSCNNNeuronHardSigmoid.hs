{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNNeuronHardSigmoid
--
-- This depends on Metal.framework
--
-- Specifies the hard sigmoid neuron filter.  For each pixel, applies the following function: f(x) = clamp((a * x) + b, 0, 1)
--
-- Generated bindings for @MPSCNNNeuronHardSigmoid@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronHardSigmoid
  ( MPSCNNNeuronHardSigmoid
  , IsMPSCNNNeuronHardSigmoid(..)
  , initWithDevice_a_b
  , initWithDevice
  , initWithDeviceSelector
  , initWithDevice_a_bSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize a neuron filter
--
-- @device@ — The device the filter will run on
--
-- @a@ — Filter property "a". See class discussion.
--
-- @b@ — Filter property "b". See class discussion.
--
-- Returns: A valid MPSCNNNeuronHardSigmoid object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:a:b:@
initWithDevice_a_b :: IsMPSCNNNeuronHardSigmoid mpscnnNeuronHardSigmoid => mpscnnNeuronHardSigmoid -> RawId -> CFloat -> CFloat -> IO (Id MPSCNNNeuronHardSigmoid)
initWithDevice_a_b mpscnnNeuronHardSigmoid device a b =
  sendOwnedMessage mpscnnNeuronHardSigmoid initWithDevice_a_bSelector device a b

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNNeuronHardSigmoid mpscnnNeuronHardSigmoid => mpscnnNeuronHardSigmoid -> RawId -> IO (Id MPSCNNNeuronHardSigmoid)
initWithDevice mpscnnNeuronHardSigmoid device =
  sendOwnedMessage mpscnnNeuronHardSigmoid initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:a:b:@
initWithDevice_a_bSelector :: Selector '[RawId, CFloat, CFloat] (Id MPSCNNNeuronHardSigmoid)
initWithDevice_a_bSelector = mkSelector "initWithDevice:a:b:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNNeuronHardSigmoid)
initWithDeviceSelector = mkSelector "initWithDevice:"

