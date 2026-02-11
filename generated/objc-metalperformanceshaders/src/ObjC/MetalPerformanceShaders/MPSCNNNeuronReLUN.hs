{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNNeuronReLUN
--
-- This depends on Metal.framework
--
-- Specifies the ReLUN neuron filter.              For each pixel, applies the following function: f(x) = [ x    , x >= 0                                                                     [ a * x, x <  0                                                                     [ b    , x >= b              As an example, the TensorFlow Relu6 activation layer can be implemented              by setting the parameter b to 6.0f:              https://www.tensorflow.org/api_docs/cc/class/tensorflow/ops/relu6.
--
-- Generated bindings for @MPSCNNNeuronReLUN@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronReLUN
  ( MPSCNNNeuronReLUN
  , IsMPSCNNNeuronReLUN(..)
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

-- | Initialize a ReLUN neuron filter
--
-- @device@ — The device the filter will run on
--
-- @a@ — Filter property "a". See class discussion.
--
-- @b@ — Filter property "b". See class discussion.
--
-- Returns: A valid MPSCNNNeuronReLUN object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:a:b:@
initWithDevice_a_b :: IsMPSCNNNeuronReLUN mpscnnNeuronReLUN => mpscnnNeuronReLUN -> RawId -> CFloat -> CFloat -> IO (Id MPSCNNNeuronReLUN)
initWithDevice_a_b mpscnnNeuronReLUN  device a b =
  sendMsg mpscnnNeuronReLUN (mkSelector "initWithDevice:a:b:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCFloat (fromIntegral a), argCFloat (fromIntegral b)] >>= ownedObject . castPtr

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNNeuronReLUN mpscnnNeuronReLUN => mpscnnNeuronReLUN -> RawId -> IO (Id MPSCNNNeuronReLUN)
initWithDevice mpscnnNeuronReLUN  device =
  sendMsg mpscnnNeuronReLUN (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:a:b:@
initWithDevice_a_bSelector :: Selector
initWithDevice_a_bSelector = mkSelector "initWithDevice:a:b:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

