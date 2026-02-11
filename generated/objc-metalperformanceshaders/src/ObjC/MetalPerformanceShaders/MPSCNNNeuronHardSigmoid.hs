{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNNeuronHardSigmoid
--
-- This depends on Metal.framework
--
-- Specifies the hard sigmoid neuron filter.  For each pixel, applies the following function: f(x) = clamp((a * x) + b, 0, 1)
--
-- Generated bindings for @MPSCNNNeuronHardSigmoid@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronHardSigmoid
  ( MPSCNNNeuronHardSigmoid
  , IsMPSCNNNeuronHardSigmoid(..)
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

-- | Initialize a neuron filter
--
-- @device@ — The device the filter will run on
--
-- @a@ — Filter property "a". See class discussion.
--
-- @b@ — Filter property "b". See class discussion.
--
-- Returns: A valid MPSCNNNeuronHardSigmoid object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:a:b:@
initWithDevice_a_b :: IsMPSCNNNeuronHardSigmoid mpscnnNeuronHardSigmoid => mpscnnNeuronHardSigmoid -> RawId -> CFloat -> CFloat -> IO (Id MPSCNNNeuronHardSigmoid)
initWithDevice_a_b mpscnnNeuronHardSigmoid  device a b =
  sendMsg mpscnnNeuronHardSigmoid (mkSelector "initWithDevice:a:b:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCFloat (fromIntegral a), argCFloat (fromIntegral b)] >>= ownedObject . castPtr

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNNeuronHardSigmoid mpscnnNeuronHardSigmoid => mpscnnNeuronHardSigmoid -> RawId -> IO (Id MPSCNNNeuronHardSigmoid)
initWithDevice mpscnnNeuronHardSigmoid  device =
  sendMsg mpscnnNeuronHardSigmoid (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:a:b:@
initWithDevice_a_bSelector :: Selector
initWithDevice_a_bSelector = mkSelector "initWithDevice:a:b:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

