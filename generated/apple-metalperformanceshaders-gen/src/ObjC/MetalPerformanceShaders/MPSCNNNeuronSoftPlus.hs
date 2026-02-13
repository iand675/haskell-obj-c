{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNNeuronSoftPlus
--
-- This depends on Metal.framework
--
-- Specifies the parametric softplus neuron filter.              For each pixel, applies the following function: f(x) = a * log(1 + e^(b * x))
--
-- Generated bindings for @MPSCNNNeuronSoftPlus@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronSoftPlus
  ( MPSCNNNeuronSoftPlus
  , IsMPSCNNNeuronSoftPlus(..)
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

-- | Initialize a parametric softplus neuron filter
--
-- @device@ — The device the filter will run on
--
-- @a@ — Filter property "a". See class discussion.
--
-- @b@ — Filter property "b". See class discussion.
--
-- Returns: A valid MPSCNNNeuronSoftPlus object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:a:b:@
initWithDevice_a_b :: IsMPSCNNNeuronSoftPlus mpscnnNeuronSoftPlus => mpscnnNeuronSoftPlus -> RawId -> CFloat -> CFloat -> IO (Id MPSCNNNeuronSoftPlus)
initWithDevice_a_b mpscnnNeuronSoftPlus device a b =
  sendOwnedMessage mpscnnNeuronSoftPlus initWithDevice_a_bSelector device a b

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNNeuronSoftPlus mpscnnNeuronSoftPlus => mpscnnNeuronSoftPlus -> RawId -> IO (Id MPSCNNNeuronSoftPlus)
initWithDevice mpscnnNeuronSoftPlus device =
  sendOwnedMessage mpscnnNeuronSoftPlus initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:a:b:@
initWithDevice_a_bSelector :: Selector '[RawId, CFloat, CFloat] (Id MPSCNNNeuronSoftPlus)
initWithDevice_a_bSelector = mkSelector "initWithDevice:a:b:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNNeuronSoftPlus)
initWithDeviceSelector = mkSelector "initWithDevice:"

