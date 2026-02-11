{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNNeuronReLU
--
-- This depends on Metal.framework
--
-- Specifies the ReLU neuron filter.              For each pixel, applies the following function: f(x) = x, if x >= 0                                                                   = a * x if x < 0              This is called Leaky ReLU in literature. Some literature defines              classical ReLU as max(0, x). If you want this behavior, simply pass a = 0
--
-- Generated bindings for @MPSCNNNeuronReLU@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronReLU
  ( MPSCNNNeuronReLU
  , IsMPSCNNNeuronReLU(..)
  , initWithDevice_a
  , initWithDevice
  , initWithDevice_aSelector
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

-- | Initialize the ReLU neuron filter
--
-- @device@ — The device the filter will run on
--
-- @a@ — Filter property "a". See class discussion.
--
-- Returns: A valid MPSCNNNeuronReLU object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:a:@
initWithDevice_a :: IsMPSCNNNeuronReLU mpscnnNeuronReLU => mpscnnNeuronReLU -> RawId -> CFloat -> IO (Id MPSCNNNeuronReLU)
initWithDevice_a mpscnnNeuronReLU  device a =
  sendMsg mpscnnNeuronReLU (mkSelector "initWithDevice:a:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCFloat (fromIntegral a)] >>= ownedObject . castPtr

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNNeuronReLU mpscnnNeuronReLU => mpscnnNeuronReLU -> RawId -> IO (Id MPSCNNNeuronReLU)
initWithDevice mpscnnNeuronReLU  device =
  sendMsg mpscnnNeuronReLU (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:a:@
initWithDevice_aSelector :: Selector
initWithDevice_aSelector = mkSelector "initWithDevice:a:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

