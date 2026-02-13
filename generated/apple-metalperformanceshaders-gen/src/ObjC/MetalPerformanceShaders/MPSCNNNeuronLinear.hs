{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNNeuronLinear
--
-- This depends on Metal.framework
--
-- Specifies the linear neuron filter. For each pixel, applies the following function: f(x) = a * x + b
--
-- Generated bindings for @MPSCNNNeuronLinear@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronLinear
  ( MPSCNNNeuronLinear
  , IsMPSCNNNeuronLinear(..)
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

-- | Initialize the linear neuron filter
--
-- @device@ — The device the filter will run on
--
-- @a@ — Filter property "a". See class discussion.
--
-- @b@ — Filter property "b". See class discussion.
--
-- Returns: A valid MPSCNNNeuronLinear object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:a:b:@
initWithDevice_a_b :: IsMPSCNNNeuronLinear mpscnnNeuronLinear => mpscnnNeuronLinear -> RawId -> CFloat -> CFloat -> IO (Id MPSCNNNeuronLinear)
initWithDevice_a_b mpscnnNeuronLinear device a b =
  sendOwnedMessage mpscnnNeuronLinear initWithDevice_a_bSelector device a b

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNNeuronLinear mpscnnNeuronLinear => mpscnnNeuronLinear -> RawId -> IO (Id MPSCNNNeuronLinear)
initWithDevice mpscnnNeuronLinear device =
  sendOwnedMessage mpscnnNeuronLinear initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:a:b:@
initWithDevice_a_bSelector :: Selector '[RawId, CFloat, CFloat] (Id MPSCNNNeuronLinear)
initWithDevice_a_bSelector = mkSelector "initWithDevice:a:b:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNNeuronLinear)
initWithDeviceSelector = mkSelector "initWithDevice:"

