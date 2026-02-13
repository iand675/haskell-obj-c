{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNNeuronAbsolute
--
-- This depends on Metal.framework
--
-- Specifies the absolute neuron filter.  For each pixel, applies the following function: f(x) = | x |
--
-- Generated bindings for @MPSCNNNeuronAbsolute@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronAbsolute
  ( MPSCNNNeuronAbsolute
  , IsMPSCNNNeuronAbsolute(..)
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
-- Returns: A valid MPSCNNNeuronAbsolute object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSCNNNeuronAbsolute mpscnnNeuronAbsolute => mpscnnNeuronAbsolute -> RawId -> IO (Id MPSCNNNeuronAbsolute)
initWithDevice mpscnnNeuronAbsolute device =
  sendOwnedMessage mpscnnNeuronAbsolute initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNNeuronAbsolute)
initWithDeviceSelector = mkSelector "initWithDevice:"

