{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNNeuronSoftSign
--
-- This depends on Metal.framework
--
-- Specifies the softsign neuron filter.              For each pixel, applies the following function: f(x) = x / (1 + abs(x))
--
-- Generated bindings for @MPSCNNNeuronSoftSign@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronSoftSign
  ( MPSCNNNeuronSoftSign
  , IsMPSCNNNeuronSoftSign(..)
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

-- | Initialize a softsign neuron filter
--
-- @device@ — The device the filter will run on
--
-- Returns: A valid MPSCNNNeuronSoftSign object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSCNNNeuronSoftSign mpscnnNeuronSoftSign => mpscnnNeuronSoftSign -> RawId -> IO (Id MPSCNNNeuronSoftSign)
initWithDevice mpscnnNeuronSoftSign device =
  sendOwnedMessage mpscnnNeuronSoftSign initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNNeuronSoftSign)
initWithDeviceSelector = mkSelector "initWithDevice:"

