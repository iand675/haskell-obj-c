{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNNeuronReLUN
--
-- This depends on Metal.framework
--
-- Specifies the ReLUN neuron filter.              For each pixel, applies the following function: f(x) = [ x    , x >= 0                                                                     [ a * x, x <  0                                                                     [ b    , x >= b              As an example, the TensorFlow Relu6 activation layer can be implemented              by setting the parameter b to 6.0f:              https://www.tensorflow.org/api_docs/cc/class/tensorflow/ops/relu6.
--
-- Generated bindings for @MPSCNNNeuronReLUN@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronReLUN
  ( MPSCNNNeuronReLUN
  , IsMPSCNNNeuronReLUN(..)
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

-- | Initialize a ReLUN neuron filter
--
-- @device@ — The device the filter will run on
--
-- @a@ — Filter property "a". See class discussion.
--
-- @b@ — Filter property "b". See class discussion.
--
-- Returns: A valid MPSCNNNeuronReLUN object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:a:b:@
initWithDevice_a_b :: IsMPSCNNNeuronReLUN mpscnnNeuronReLUN => mpscnnNeuronReLUN -> RawId -> CFloat -> CFloat -> IO (Id MPSCNNNeuronReLUN)
initWithDevice_a_b mpscnnNeuronReLUN device a b =
  sendOwnedMessage mpscnnNeuronReLUN initWithDevice_a_bSelector device a b

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNNeuronReLUN mpscnnNeuronReLUN => mpscnnNeuronReLUN -> RawId -> IO (Id MPSCNNNeuronReLUN)
initWithDevice mpscnnNeuronReLUN device =
  sendOwnedMessage mpscnnNeuronReLUN initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:a:b:@
initWithDevice_a_bSelector :: Selector '[RawId, CFloat, CFloat] (Id MPSCNNNeuronReLUN)
initWithDevice_a_bSelector = mkSelector "initWithDevice:a:b:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNNeuronReLUN)
initWithDeviceSelector = mkSelector "initWithDevice:"

