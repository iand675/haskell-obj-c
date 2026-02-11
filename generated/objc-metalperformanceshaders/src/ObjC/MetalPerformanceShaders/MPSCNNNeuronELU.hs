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
initWithDevice_a mpscnnNeuronELU  device a =
  sendMsg mpscnnNeuronELU (mkSelector "initWithDevice:a:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCFloat (fromIntegral a)] >>= ownedObject . castPtr

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNNeuronELU mpscnnNeuronELU => mpscnnNeuronELU -> RawId -> IO (Id MPSCNNNeuronELU)
initWithDevice mpscnnNeuronELU  device =
  sendMsg mpscnnNeuronELU (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:a:@
initWithDevice_aSelector :: Selector
initWithDevice_aSelector = mkSelector "initWithDevice:a:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

