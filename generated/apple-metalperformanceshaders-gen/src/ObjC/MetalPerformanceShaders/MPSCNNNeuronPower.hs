{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNNeuronPower
--
-- This depends on Metal.framework.
--
-- Specifies the Power neuron filter.              For each pixel, applies the following function: f(x) = (a * x + b) ^ c.
--
-- Generated bindings for @MPSCNNNeuronPower@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronPower
  ( MPSCNNNeuronPower
  , IsMPSCNNNeuronPower(..)
  , initWithDevice_a_b_c
  , initWithDevice
  , initWithDeviceSelector
  , initWithDevice_a_b_cSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize a Power neuron filter.
--
-- @device@ — The device the filter will run on.
--
-- @a@ — Filter property "a". See class discussion.
--
-- @b@ — Filter property "b". See class discussion.
--
-- @c@ — Filter property "c". See class discussion.
--
-- Returns: A valid MPSCNNNeuronPower object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:a:b:c:@
initWithDevice_a_b_c :: IsMPSCNNNeuronPower mpscnnNeuronPower => mpscnnNeuronPower -> RawId -> CFloat -> CFloat -> CFloat -> IO (Id MPSCNNNeuronPower)
initWithDevice_a_b_c mpscnnNeuronPower device a b c =
  sendOwnedMessage mpscnnNeuronPower initWithDevice_a_b_cSelector device a b c

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNNeuronPower mpscnnNeuronPower => mpscnnNeuronPower -> RawId -> IO (Id MPSCNNNeuronPower)
initWithDevice mpscnnNeuronPower device =
  sendOwnedMessage mpscnnNeuronPower initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:a:b:c:@
initWithDevice_a_b_cSelector :: Selector '[RawId, CFloat, CFloat, CFloat] (Id MPSCNNNeuronPower)
initWithDevice_a_b_cSelector = mkSelector "initWithDevice:a:b:c:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNNeuronPower)
initWithDeviceSelector = mkSelector "initWithDevice:"

