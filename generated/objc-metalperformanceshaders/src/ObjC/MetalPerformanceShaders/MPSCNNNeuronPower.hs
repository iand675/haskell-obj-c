{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNNeuronPower
--
-- This depends on Metal.framework.
--
-- Specifies the Power neuron filter.              For each pixel, applies the following function: f(x) = (a * x + b) ^ c.
--
-- Generated bindings for @MPSCNNNeuronPower@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronPower
  ( MPSCNNNeuronPower
  , IsMPSCNNNeuronPower(..)
  , initWithDevice_a_b_c
  , initWithDevice
  , initWithDevice_a_b_cSelector
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

-- | Initialize a Power neuron filter.
--
-- @device@ — The device the filter will run on.
--
-- @a@ — Filter property "a". See class discussion.
--
-- @b@ — Filter property "b". See class discussion.
--
-- @c@ — Filter property "c". See class discussion.
--
-- Returns: A valid MPSCNNNeuronPower object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:a:b:c:@
initWithDevice_a_b_c :: IsMPSCNNNeuronPower mpscnnNeuronPower => mpscnnNeuronPower -> RawId -> CFloat -> CFloat -> CFloat -> IO (Id MPSCNNNeuronPower)
initWithDevice_a_b_c mpscnnNeuronPower  device a b c =
  sendMsg mpscnnNeuronPower (mkSelector "initWithDevice:a:b:c:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCFloat (fromIntegral a), argCFloat (fromIntegral b), argCFloat (fromIntegral c)] >>= ownedObject . castPtr

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNNeuronPower mpscnnNeuronPower => mpscnnNeuronPower -> RawId -> IO (Id MPSCNNNeuronPower)
initWithDevice mpscnnNeuronPower  device =
  sendMsg mpscnnNeuronPower (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:a:b:c:@
initWithDevice_a_b_cSelector :: Selector
initWithDevice_a_b_cSelector = mkSelector "initWithDevice:a:b:c:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

