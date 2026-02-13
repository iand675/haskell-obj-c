{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNNeuronDescriptor
--
-- This depends on Metal.framework
--
-- The MPSNNNeuronDescriptor specifies a neuron descriptor.              Supported neuron types:
--
-- Neuron type "none": f(x) = x              Parameters: none
--
-- ReLU neuron filter: f(x) = x >= 0 ? x : a * x              This is called Leaky ReLU in literature. Some literature defines              classical ReLU as max(0, x). If you want this behavior, simply pass a = 0.              Parameters: a              For default behavior, set the value of a to 0.0f.
--
-- Linear neuron filter: f(x) = a * x + b              Parameters: a, b              For default behavior, set the value of a to 1.0f and the value of b to 0.0f.
--
-- Sigmoid neuron filter: f(x) = 1 / (1 + e^-x)              Parameters: none
--
-- Hard Sigmoid filter: f(x) = clamp((x * a) + b, 0, 1)              Parameters: a, b              For default behavior, set the value of a to 0.2f and the value of b to 0.5f.
--
-- Hyperbolic tangent (TanH) neuron filter: f(x) = a * tanh(b * x)              Parameters: a, b              For default behavior, set the value of a to 1.0f and the value of b to 1.0f.
--
-- Absolute neuron filter: f(x) = fabs(x)              Parameters: none
--
-- Parametric Soft Plus neuron filter: f(x) = a * log(1 + e^(b * x))              Parameters: a, b              For default behavior, set the value of a to 1.0f and the value of b to 1.0f.
--
-- Parametric Soft Sign neuron filter: f(x) = x / (1 + abs(x))              Parameters: none
--
-- Parametric ELU neuron filter: f(x) = x >= 0 ? x : a * (exp(x) - 1)              Parameters: a              For default behavior, set the value of a to 1.0f.
--
-- Parametric ReLU (PReLU) neuron filter: Same as ReLU, except parameter              aArray is per channel.              For each pixel, applies the following function: f(x_i) = x_i, if x_i >= 0                                                                     = a_i * x_i if x_i < 0              i in [0...channels-1]              i.e. parameters a_i are learned and applied to each channel separately. Compare              this to ReLu where parameter a is shared across all channels.              See https://arxiv.org/pdf/1502.01852.pdf for details.              Parameters: aArray - Array of floats containing per channel value of PReLu parameter                          count - Number of float values in array aArray.
--
-- ReLUN neuron filter: f(x) = min((x >= 0 ? x : a * x), b)              Parameters: a, b              As an example, the TensorFlow Relu6 activation layer can be implemented              by setting the parameter b to 6.0f:              https://www.tensorflow.org/api_docs/cc/class/tensorflow/ops/relu6.              For default behavior, set the value of a to 1.0f and the value of b to 6.0f.
--
-- Generated bindings for @MPSNNNeuronDescriptor@.
module ObjC.MetalPerformanceShaders.MPSNNNeuronDescriptor
  ( MPSNNNeuronDescriptor
  , IsMPSNNNeuronDescriptor(..)
  , init_
  , cnnNeuronDescriptorWithType
  , cnnNeuronDescriptorWithType_a
  , cnnNeuronDescriptorWithType_a_b
  , cnnNeuronDescriptorWithType_a_b_c
  , neuronType
  , setNeuronType
  , a
  , setA
  , b
  , setB
  , c
  , setC
  , data_
  , setData
  , aSelector
  , bSelector
  , cSelector
  , cnnNeuronDescriptorWithTypeSelector
  , cnnNeuronDescriptorWithType_aSelector
  , cnnNeuronDescriptorWithType_a_bSelector
  , cnnNeuronDescriptorWithType_a_b_cSelector
  , dataSelector
  , initSelector
  , neuronTypeSelector
  , setASelector
  , setBSelector
  , setCSelector
  , setDataSelector
  , setNeuronTypeSelector

  -- * Enum types
  , MPSCNNNeuronType(MPSCNNNeuronType)
  , pattern MPSCNNNeuronTypeNone
  , pattern MPSCNNNeuronTypeReLU
  , pattern MPSCNNNeuronTypeLinear
  , pattern MPSCNNNeuronTypeSigmoid
  , pattern MPSCNNNeuronTypeHardSigmoid
  , pattern MPSCNNNeuronTypeTanH
  , pattern MPSCNNNeuronTypeAbsolute
  , pattern MPSCNNNeuronTypeSoftPlus
  , pattern MPSCNNNeuronTypeSoftSign
  , pattern MPSCNNNeuronTypeELU
  , pattern MPSCNNNeuronTypePReLU
  , pattern MPSCNNNeuronTypeReLUN
  , pattern MPSCNNNeuronTypePower
  , pattern MPSCNNNeuronTypeExponential
  , pattern MPSCNNNeuronTypeLogarithm
  , pattern MPSCNNNeuronTypeGeLU
  , pattern MPSCNNNeuronTypeCount

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | You must use one of the interfaces below instead.
--
-- ObjC selector: @- init@
init_ :: IsMPSNNNeuronDescriptor mpsnnNeuronDescriptor => mpsnnNeuronDescriptor -> IO (Id MPSNNNeuronDescriptor)
init_ mpsnnNeuronDescriptor =
  sendOwnedMessage mpsnnNeuronDescriptor initSelector

-- | Make a descriptor for a MPSCNNNeuron object.
--
-- @neuronType@ — The type of a neuron filter.
--
-- Returns: A valid MPSNNNeuronDescriptor object or nil, if failure.
--
-- ObjC selector: @+ cnnNeuronDescriptorWithType:@
cnnNeuronDescriptorWithType :: MPSCNNNeuronType -> IO (Id MPSNNNeuronDescriptor)
cnnNeuronDescriptorWithType neuronType =
  do
    cls' <- getRequiredClass "MPSNNNeuronDescriptor"
    sendClassMessage cls' cnnNeuronDescriptorWithTypeSelector neuronType

-- | Make a descriptor for a MPSCNNNeuron object.
--
-- @neuronType@ — The type of a neuron filter.
--
-- @a@ — Parameter "a".
--
-- Returns: A valid MPSNNNeuronDescriptor object or nil, if failure.
--
-- ObjC selector: @+ cnnNeuronDescriptorWithType:a:@
cnnNeuronDescriptorWithType_a :: MPSCNNNeuronType -> CFloat -> IO (Id MPSNNNeuronDescriptor)
cnnNeuronDescriptorWithType_a neuronType a =
  do
    cls' <- getRequiredClass "MPSNNNeuronDescriptor"
    sendClassMessage cls' cnnNeuronDescriptorWithType_aSelector neuronType a

-- | Initialize the neuron descriptor.
--
-- @neuronType@ — The type of a neuron filter.
--
-- @a@ — Parameter "a".
--
-- @b@ — Parameter "b".
--
-- Returns: A valid MPSNNNeuronDescriptor object or nil, if failure.
--
-- ObjC selector: @+ cnnNeuronDescriptorWithType:a:b:@
cnnNeuronDescriptorWithType_a_b :: MPSCNNNeuronType -> CFloat -> CFloat -> IO (Id MPSNNNeuronDescriptor)
cnnNeuronDescriptorWithType_a_b neuronType a b =
  do
    cls' <- getRequiredClass "MPSNNNeuronDescriptor"
    sendClassMessage cls' cnnNeuronDescriptorWithType_a_bSelector neuronType a b

-- | Make a descriptor for a MPSCNNNeuron object.
--
-- @neuronType@ — The type of a neuron filter.
--
-- @a@ — Parameter "a".
--
-- @b@ — Parameter "b".
--
-- @c@ — Parameter "c".
--
-- Returns: A valid MPSNNNeuronDescriptor object or nil, if failure.
--
-- ObjC selector: @+ cnnNeuronDescriptorWithType:a:b:c:@
cnnNeuronDescriptorWithType_a_b_c :: MPSCNNNeuronType -> CFloat -> CFloat -> CFloat -> IO (Id MPSNNNeuronDescriptor)
cnnNeuronDescriptorWithType_a_b_c neuronType a b c =
  do
    cls' <- getRequiredClass "MPSNNNeuronDescriptor"
    sendClassMessage cls' cnnNeuronDescriptorWithType_a_b_cSelector neuronType a b c

-- | @- neuronType@
neuronType :: IsMPSNNNeuronDescriptor mpsnnNeuronDescriptor => mpsnnNeuronDescriptor -> IO MPSCNNNeuronType
neuronType mpsnnNeuronDescriptor =
  sendMessage mpsnnNeuronDescriptor neuronTypeSelector

-- | @- setNeuronType:@
setNeuronType :: IsMPSNNNeuronDescriptor mpsnnNeuronDescriptor => mpsnnNeuronDescriptor -> MPSCNNNeuronType -> IO ()
setNeuronType mpsnnNeuronDescriptor value =
  sendMessage mpsnnNeuronDescriptor setNeuronTypeSelector value

-- | @- a@
a :: IsMPSNNNeuronDescriptor mpsnnNeuronDescriptor => mpsnnNeuronDescriptor -> IO CFloat
a mpsnnNeuronDescriptor =
  sendMessage mpsnnNeuronDescriptor aSelector

-- | @- setA:@
setA :: IsMPSNNNeuronDescriptor mpsnnNeuronDescriptor => mpsnnNeuronDescriptor -> CFloat -> IO ()
setA mpsnnNeuronDescriptor value =
  sendMessage mpsnnNeuronDescriptor setASelector value

-- | @- b@
b :: IsMPSNNNeuronDescriptor mpsnnNeuronDescriptor => mpsnnNeuronDescriptor -> IO CFloat
b mpsnnNeuronDescriptor =
  sendMessage mpsnnNeuronDescriptor bSelector

-- | @- setB:@
setB :: IsMPSNNNeuronDescriptor mpsnnNeuronDescriptor => mpsnnNeuronDescriptor -> CFloat -> IO ()
setB mpsnnNeuronDescriptor value =
  sendMessage mpsnnNeuronDescriptor setBSelector value

-- | @- c@
c :: IsMPSNNNeuronDescriptor mpsnnNeuronDescriptor => mpsnnNeuronDescriptor -> IO CFloat
c mpsnnNeuronDescriptor =
  sendMessage mpsnnNeuronDescriptor cSelector

-- | @- setC:@
setC :: IsMPSNNNeuronDescriptor mpsnnNeuronDescriptor => mpsnnNeuronDescriptor -> CFloat -> IO ()
setC mpsnnNeuronDescriptor value =
  sendMessage mpsnnNeuronDescriptor setCSelector value

-- | @- data@
data_ :: IsMPSNNNeuronDescriptor mpsnnNeuronDescriptor => mpsnnNeuronDescriptor -> IO (Id NSData)
data_ mpsnnNeuronDescriptor =
  sendMessage mpsnnNeuronDescriptor dataSelector

-- | @- setData:@
setData :: (IsMPSNNNeuronDescriptor mpsnnNeuronDescriptor, IsNSData value) => mpsnnNeuronDescriptor -> value -> IO ()
setData mpsnnNeuronDescriptor value =
  sendMessage mpsnnNeuronDescriptor setDataSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPSNNNeuronDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @cnnNeuronDescriptorWithType:@
cnnNeuronDescriptorWithTypeSelector :: Selector '[MPSCNNNeuronType] (Id MPSNNNeuronDescriptor)
cnnNeuronDescriptorWithTypeSelector = mkSelector "cnnNeuronDescriptorWithType:"

-- | @Selector@ for @cnnNeuronDescriptorWithType:a:@
cnnNeuronDescriptorWithType_aSelector :: Selector '[MPSCNNNeuronType, CFloat] (Id MPSNNNeuronDescriptor)
cnnNeuronDescriptorWithType_aSelector = mkSelector "cnnNeuronDescriptorWithType:a:"

-- | @Selector@ for @cnnNeuronDescriptorWithType:a:b:@
cnnNeuronDescriptorWithType_a_bSelector :: Selector '[MPSCNNNeuronType, CFloat, CFloat] (Id MPSNNNeuronDescriptor)
cnnNeuronDescriptorWithType_a_bSelector = mkSelector "cnnNeuronDescriptorWithType:a:b:"

-- | @Selector@ for @cnnNeuronDescriptorWithType:a:b:c:@
cnnNeuronDescriptorWithType_a_b_cSelector :: Selector '[MPSCNNNeuronType, CFloat, CFloat, CFloat] (Id MPSNNNeuronDescriptor)
cnnNeuronDescriptorWithType_a_b_cSelector = mkSelector "cnnNeuronDescriptorWithType:a:b:c:"

-- | @Selector@ for @neuronType@
neuronTypeSelector :: Selector '[] MPSCNNNeuronType
neuronTypeSelector = mkSelector "neuronType"

-- | @Selector@ for @setNeuronType:@
setNeuronTypeSelector :: Selector '[MPSCNNNeuronType] ()
setNeuronTypeSelector = mkSelector "setNeuronType:"

-- | @Selector@ for @a@
aSelector :: Selector '[] CFloat
aSelector = mkSelector "a"

-- | @Selector@ for @setA:@
setASelector :: Selector '[CFloat] ()
setASelector = mkSelector "setA:"

-- | @Selector@ for @b@
bSelector :: Selector '[] CFloat
bSelector = mkSelector "b"

-- | @Selector@ for @setB:@
setBSelector :: Selector '[CFloat] ()
setBSelector = mkSelector "setB:"

-- | @Selector@ for @c@
cSelector :: Selector '[] CFloat
cSelector = mkSelector "c"

-- | @Selector@ for @setC:@
setCSelector :: Selector '[CFloat] ()
setCSelector = mkSelector "setC:"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector '[Id NSData] ()
setDataSelector = mkSelector "setData:"

