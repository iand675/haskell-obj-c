{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNNeuronGradient
--
-- This depends on Metal.framework
--
-- This filter is a backward filter for the neuron activation function filter.
--
-- The following filter types are supported:  MPSCNNNeuronTypeNone            ///< df/dx = 1  MPSCNNNeuronTypeLinear          ///< df/dx = a  MPSCNNNeuronTypeReLU            ///< df/dx = [ 1, if x >= 0                                               [ a, if x <  0  MPSCNNNeuronTypeSigmoid         ///< df/dx = e^x / (e^x + 1)^2  MPSCNNNeuronTypeHardSigmoid     ///< df/dx = [ a, if ((x * a) + b >= 0) and ((x * a) + b <= 1)                                               [ 0, otherwise  MPSCNNNeuronTypeTanH            ///< df/dx = a * b * (1 - tanh^2(b * x))  MPSCNNNeuronTypeAbsolute        ///< df/dx = sign(x)  MPSCNNNeuronTypeSoftPlus        ///< df/dx = (a * b * exp(b * x)) / (exp(b * x) + 1)  MPSCNNNeuronTypeSoftSign        ///< df/dx = 1 / (|x| + 1)^2  MPSCNNNeuronTypeELU             ///< df/dx = [ a * exp(x), x <  0                                               [          1, x >= 0  MPSCNNNeuronTypePReLU           ///< df/dx = [  1, if x >= 0                                               [ aV, if x <  0  MPSCNNNeuronTypeReLUN           ///< df/dx = [ 1, if x >= 0                                               [ a, if x <  0                                               [ b, if x >= b  MPSCNNNeuronTypePower           ///< df/dx = a * c * (a * x + b)^(c - 1)  MPSCNNNeuronTypeExponential     ///< df/dx = [         a * exp(a * x + b), if c == -1                                               [ a * log(c) * c^(a * x + b), if c != -1  MPSCNNNeuronTypeLogarithm       ///< df/dx = [            a / (a * in + b), if c == -1                                               [ a / (log(c) * (a * in + b)), if c != -1  MPSCNNNeuronTypeGeLU            ///< df/dx = 0.5 * (1.0 + erf(x * sqrt(0.5))) + (sqrt(0.5) * M_2_SQRTPI * exp(-x*x * 0.5) * x) )
--
-- The result of the above operation is multiplied with the gradient, computed by the preceeding filter (going backwards).
--
-- Generated bindings for @MPSCNNNeuronGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuronGradient
  ( MPSCNNNeuronGradient
  , IsMPSCNNNeuronGradient(..)
  , initWithDevice
  , initWithDevice_neuronDescriptor
  , initWithCoder_device
  , neuronType
  , a
  , b
  , c
  , data_
  , aSelector
  , bSelector
  , cSelector
  , dataSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_neuronDescriptorSelector
  , neuronTypeSelector

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

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNNeuronGradient mpscnnNeuronGradient => mpscnnNeuronGradient -> RawId -> IO (Id MPSCNNNeuronGradient)
initWithDevice mpscnnNeuronGradient device =
  sendOwnedMessage mpscnnNeuronGradient initWithDeviceSelector device

-- | Initialize the neuron gradient filter with a neuron descriptor.
--
-- @device@ — The device the filter will run on.
--
-- @neuronDescriptor@ — The neuron descriptor.                                      For the neuron of type MPSCNNNeuronTypePReLU, the neuron                                      descriptor references an NSData object containing a float array                                      with the per feature channel value of PReLu parameter and, in this                                      case, the MPSCNNNeuronGradient retains the NSData object.
--
-- Returns: A valid MPSCNNNeuronGradient object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:neuronDescriptor:@
initWithDevice_neuronDescriptor :: (IsMPSCNNNeuronGradient mpscnnNeuronGradient, IsMPSNNNeuronDescriptor neuronDescriptor) => mpscnnNeuronGradient -> RawId -> neuronDescriptor -> IO (Id MPSCNNNeuronGradient)
initWithDevice_neuronDescriptor mpscnnNeuronGradient device neuronDescriptor =
  sendOwnedMessage mpscnnNeuronGradient initWithDevice_neuronDescriptorSelector device (toMPSNNNeuronDescriptor neuronDescriptor)

-- | NSSecureCoding compatability
--
-- While the standard NSSecureCoding/NSCoding method              -initWithCoder: should work, since the file can't              know which device your data is allocated on, we              have to guess and may guess incorrectly.  To avoid              that problem, use initWithCoder:device instead.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSKernel
--
-- @device@ — The MTLDevice on which to make the MPSKernel
--
-- Returns: A new MPSKernel object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNNeuronGradient mpscnnNeuronGradient, IsNSCoder aDecoder) => mpscnnNeuronGradient -> aDecoder -> RawId -> IO (Id MPSCNNNeuronGradient)
initWithCoder_device mpscnnNeuronGradient aDecoder device =
  sendOwnedMessage mpscnnNeuronGradient initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | @- neuronType@
neuronType :: IsMPSCNNNeuronGradient mpscnnNeuronGradient => mpscnnNeuronGradient -> IO MPSCNNNeuronType
neuronType mpscnnNeuronGradient =
  sendMessage mpscnnNeuronGradient neuronTypeSelector

-- | @- a@
a :: IsMPSCNNNeuronGradient mpscnnNeuronGradient => mpscnnNeuronGradient -> IO CFloat
a mpscnnNeuronGradient =
  sendMessage mpscnnNeuronGradient aSelector

-- | @- b@
b :: IsMPSCNNNeuronGradient mpscnnNeuronGradient => mpscnnNeuronGradient -> IO CFloat
b mpscnnNeuronGradient =
  sendMessage mpscnnNeuronGradient bSelector

-- | @- c@
c :: IsMPSCNNNeuronGradient mpscnnNeuronGradient => mpscnnNeuronGradient -> IO CFloat
c mpscnnNeuronGradient =
  sendMessage mpscnnNeuronGradient cSelector

-- | @- data@
data_ :: IsMPSCNNNeuronGradient mpscnnNeuronGradient => mpscnnNeuronGradient -> IO (Id NSData)
data_ mpscnnNeuronGradient =
  sendMessage mpscnnNeuronGradient dataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNNeuronGradient)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:neuronDescriptor:@
initWithDevice_neuronDescriptorSelector :: Selector '[RawId, Id MPSNNNeuronDescriptor] (Id MPSCNNNeuronGradient)
initWithDevice_neuronDescriptorSelector = mkSelector "initWithDevice:neuronDescriptor:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNNeuronGradient)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @neuronType@
neuronTypeSelector :: Selector '[] MPSCNNNeuronType
neuronTypeSelector = mkSelector "neuronType"

-- | @Selector@ for @a@
aSelector :: Selector '[] CFloat
aSelector = mkSelector "a"

-- | @Selector@ for @b@
bSelector :: Selector '[] CFloat
bSelector = mkSelector "b"

-- | @Selector@ for @c@
cSelector :: Selector '[] CFloat
cSelector = mkSelector "c"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

