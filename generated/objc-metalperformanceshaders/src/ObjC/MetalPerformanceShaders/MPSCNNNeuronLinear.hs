{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNNeuronLinear
--
-- This depends on Metal.framework
--
-- Specifies the linear neuron filter. For each pixel, applies the following function: f(x) = a * x + b
--
-- Generated bindings for @MPSCNNNeuronLinear@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronLinear
  ( MPSCNNNeuronLinear
  , IsMPSCNNNeuronLinear(..)
  , initWithDevice_a_b
  , initWithDevice
  , initWithDevice_a_bSelector
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

-- | Initialize the linear neuron filter
--
-- @device@ — The device the filter will run on
--
-- @a@ — Filter property "a". See class discussion.
--
-- @b@ — Filter property "b". See class discussion.
--
-- Returns: A valid MPSCNNNeuronLinear object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:a:b:@
initWithDevice_a_b :: IsMPSCNNNeuronLinear mpscnnNeuronLinear => mpscnnNeuronLinear -> RawId -> CFloat -> CFloat -> IO (Id MPSCNNNeuronLinear)
initWithDevice_a_b mpscnnNeuronLinear  device a b =
  sendMsg mpscnnNeuronLinear (mkSelector "initWithDevice:a:b:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCFloat (fromIntegral a), argCFloat (fromIntegral b)] >>= ownedObject . castPtr

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNNeuronLinear mpscnnNeuronLinear => mpscnnNeuronLinear -> RawId -> IO (Id MPSCNNNeuronLinear)
initWithDevice mpscnnNeuronLinear  device =
  sendMsg mpscnnNeuronLinear (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:a:b:@
initWithDevice_a_bSelector :: Selector
initWithDevice_a_bSelector = mkSelector "initWithDevice:a:b:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

