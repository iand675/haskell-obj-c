{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNNeuronTanH
--
-- This depends on Metal.framework
--
-- Specifies the hyperbolic tangent neuron filter.              For each pixel, applies the following function: f(x) = a * tanh(b * x)
--
-- Generated bindings for @MPSCNNNeuronTanH@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronTanH
  ( MPSCNNNeuronTanH
  , IsMPSCNNNeuronTanH(..)
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

-- | Initialize the hyperbolic tangent neuron filter
--
-- @device@ — The device the filter will run on
--
-- @a@ — Filter property "a". See class discussion.
--
-- @b@ — Filter property "b". See class discussion.
--
-- Returns: A valid MPSCNNNeuronTanH object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:a:b:@
initWithDevice_a_b :: IsMPSCNNNeuronTanH mpscnnNeuronTanH => mpscnnNeuronTanH -> RawId -> CFloat -> CFloat -> IO (Id MPSCNNNeuronTanH)
initWithDevice_a_b mpscnnNeuronTanH device a b =
  sendOwnedMessage mpscnnNeuronTanH initWithDevice_a_bSelector device a b

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNNeuronTanH mpscnnNeuronTanH => mpscnnNeuronTanH -> RawId -> IO (Id MPSCNNNeuronTanH)
initWithDevice mpscnnNeuronTanH device =
  sendOwnedMessage mpscnnNeuronTanH initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:a:b:@
initWithDevice_a_bSelector :: Selector '[RawId, CFloat, CFloat] (Id MPSCNNNeuronTanH)
initWithDevice_a_bSelector = mkSelector "initWithDevice:a:b:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNNeuronTanH)
initWithDeviceSelector = mkSelector "initWithDevice:"

