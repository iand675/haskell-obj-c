{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNNeuronLogarithm
--
-- This depends on Metal.framework.
--
-- Specifies the Logarithm neuron filter.              For each pixel, applies the following function: f(x) = log_c(a * x + b).
--
-- If the value of c is -1.0f, the base (c) is set to e.
--
-- Generated bindings for @MPSCNNNeuronLogarithm@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronLogarithm
  ( MPSCNNNeuronLogarithm
  , IsMPSCNNNeuronLogarithm(..)
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

-- | Initialize a Logarithm neuron filter.
--
-- @device@ — The device the filter will run on.
--
-- @a@ — Filter property "a". See class discussion.
--
-- @b@ — Filter property "b". See class discussion.
--
-- @c@ — Filter property "c". See class discussion.
--
-- Returns: A valid MPSCNNNeuronLogarithm object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:a:b:c:@
initWithDevice_a_b_c :: IsMPSCNNNeuronLogarithm mpscnnNeuronLogarithm => mpscnnNeuronLogarithm -> RawId -> CFloat -> CFloat -> CFloat -> IO (Id MPSCNNNeuronLogarithm)
initWithDevice_a_b_c mpscnnNeuronLogarithm device a b c =
  sendOwnedMessage mpscnnNeuronLogarithm initWithDevice_a_b_cSelector device a b c

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNNeuronLogarithm mpscnnNeuronLogarithm => mpscnnNeuronLogarithm -> RawId -> IO (Id MPSCNNNeuronLogarithm)
initWithDevice mpscnnNeuronLogarithm device =
  sendOwnedMessage mpscnnNeuronLogarithm initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:a:b:c:@
initWithDevice_a_b_cSelector :: Selector '[RawId, CFloat, CFloat, CFloat] (Id MPSCNNNeuronLogarithm)
initWithDevice_a_b_cSelector = mkSelector "initWithDevice:a:b:c:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNNeuronLogarithm)
initWithDeviceSelector = mkSelector "initWithDevice:"

