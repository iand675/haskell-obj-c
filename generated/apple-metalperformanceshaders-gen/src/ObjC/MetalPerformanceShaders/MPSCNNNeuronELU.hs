{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNNeuronELU
--
-- This depends on Metal.framework
--
-- Specifies the parametric ELU neuron filter.              For each pixel, applies the following function: f(x) = [ a * (exp(x) - 1), x <  0                                                                     [ x               , x >= 0
--
-- Generated bindings for @MPSCNNNeuronELU@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronELU
  ( MPSCNNNeuronELU
  , IsMPSCNNNeuronELU(..)
  , initWithDevice_a
  , initWithDevice
  , initWithDeviceSelector
  , initWithDevice_aSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize a parametric ELU neuron filter
--
-- @device@ — The device the filter will run on
--
-- @a@ — Filter property "a". See class discussion.
--
-- Returns: A valid MPSCNNNeuronELU object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:a:@
initWithDevice_a :: IsMPSCNNNeuronELU mpscnnNeuronELU => mpscnnNeuronELU -> RawId -> CFloat -> IO (Id MPSCNNNeuronELU)
initWithDevice_a mpscnnNeuronELU device a =
  sendOwnedMessage mpscnnNeuronELU initWithDevice_aSelector device a

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNNeuronELU mpscnnNeuronELU => mpscnnNeuronELU -> RawId -> IO (Id MPSCNNNeuronELU)
initWithDevice mpscnnNeuronELU device =
  sendOwnedMessage mpscnnNeuronELU initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:a:@
initWithDevice_aSelector :: Selector '[RawId, CFloat] (Id MPSCNNNeuronELU)
initWithDevice_aSelector = mkSelector "initWithDevice:a:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNNeuronELU)
initWithDeviceSelector = mkSelector "initWithDevice:"

