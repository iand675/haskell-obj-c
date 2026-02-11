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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
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
initWithDevice mpscnnNeuronSoftSign  device =
  sendMsg mpscnnNeuronSoftSign (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

