{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNNeuronPReLU
--
-- This depends on Metal.framework
--
-- Specifies the parametric ReLU neuron filter.              For each pixel, applies the following function: f(x_i) = x_i, if x_i >= 0                                                                     = a_i * x_i if x_i < 0              i in [0...channels-1]              i.e. parameters a_i are learned and applied to each channel separately. Compare              this to ReLu where parameter a is shared across all channels.              See https://arxiv.org/pdf/1502.01852.pdf for details.
--
-- Generated bindings for @MPSCNNNeuronPReLU@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronPReLU
  ( MPSCNNNeuronPReLU
  , IsMPSCNNNeuronPReLU(..)
  , initWithDevice_a_count
  , initWithDevice
  , initWithDeviceSelector
  , initWithDevice_a_countSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize the PReLU neuron filter
--
-- @device@ — The device the filter will run on
--
-- @a@ — Array of floats containing per channel value of PReLu parameter
--
-- @count@ — Number of float values in array a.                              This usually corresponds to number of output channels in convolution layer
--
-- Returns: A valid MPSCNNNeuronPReLU object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:a:count:@
initWithDevice_a_count :: IsMPSCNNNeuronPReLU mpscnnNeuronPReLU => mpscnnNeuronPReLU -> RawId -> Const (Ptr CFloat) -> CULong -> IO (Id MPSCNNNeuronPReLU)
initWithDevice_a_count mpscnnNeuronPReLU device a count =
  sendOwnedMessage mpscnnNeuronPReLU initWithDevice_a_countSelector device a count

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNNeuronPReLU mpscnnNeuronPReLU => mpscnnNeuronPReLU -> RawId -> IO (Id MPSCNNNeuronPReLU)
initWithDevice mpscnnNeuronPReLU device =
  sendOwnedMessage mpscnnNeuronPReLU initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:a:count:@
initWithDevice_a_countSelector :: Selector '[RawId, Const (Ptr CFloat), CULong] (Id MPSCNNNeuronPReLU)
initWithDevice_a_countSelector = mkSelector "initWithDevice:a:count:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNNeuronPReLU)
initWithDeviceSelector = mkSelector "initWithDevice:"

