{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNNeuronSigmoid
--
-- This depends on Metal.framework
--
-- Specifies the sigmoid neuron filter.  For each pixel, applies the following function: f(x) = 1 / (1 + e^-x)
--
-- Generated bindings for @MPSCNNNeuronSigmoid@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronSigmoid
  ( MPSCNNNeuronSigmoid
  , IsMPSCNNNeuronSigmoid(..)
  , initWithDevice
  , initWithDeviceSelector


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
-- Returns: A valid MPSCNNNeuronSigmoid object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSCNNNeuronSigmoid mpscnnNeuronSigmoid => mpscnnNeuronSigmoid -> RawId -> IO (Id MPSCNNNeuronSigmoid)
initWithDevice mpscnnNeuronSigmoid device =
  sendOwnedMessage mpscnnNeuronSigmoid initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNNeuronSigmoid)
initWithDeviceSelector = mkSelector "initWithDevice:"

