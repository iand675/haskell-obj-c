{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNNeuron
--
-- This depends on Metal.framework
--
-- This filter applies a neuron activation function.              You must use one of the sub-classes of MPSCNNNeuron.
--
-- The following filter types are supported:  MPSCNNNeuronTypeNone            ///< f(x) = x  MPSCNNNeuronTypeLinear          ///< f(x) = a * x + b  MPSCNNNeuronTypeReLU            ///< f(x) = x >= 0 ? x : a * x  MPSCNNNeuronTypeSigmoid         ///< f(x) = 1 / (1 + e^-x)  MPSCNNNeuronTypeHardSigmoid     ///< f(x) = clamp((x * a) + b, 0, 1)  MPSCNNNeuronTypeTanH            ///< f(x) = a * tanh(b * x)  MPSCNNNeuronTypeAbsolute        ///< f(x) = fabs(x)  MPSCNNNeuronTypeSoftPlus        ///< f(x) = a * log(1 + e^(b * x))  MPSCNNNeuronTypeSoftSign        ///< f(x) = x / (1 + abs(x))  MPSCNNNeuronTypeELU             ///< f(x) = x >= 0 ? x : a * (exp(x) - 1)  MPSCNNNeuronTypePReLU           ///< Same as ReLU except parameter a is per channel  MPSCNNNeuronTypeReLUN           ///< f(x) = min((x >= 0 ? x : a * x), b)  MPSCNNNeuronTypePower           ///< f(x) = (a * x + b) ^ c  MPSCNNNeuronTypeExponential     ///< f(x) = c ^ (a * x + b)  MPSCNNNeuronTypeLogarithm       ///< f(x) = log_c(a * x + b)  MPSCNNNeuronTypeGeLU            ///< f(x) = (1.0 + erf(x * sqrt(0.5))) * 0.5 * x
--
-- Generated bindings for @MPSCNNNeuron@.
module ObjC.MetalPerformanceShaders.MPSCNNNeuron
  ( MPSCNNNeuron
  , IsMPSCNNNeuron(..)
  , initWithDevice
  , initWithDevice_neuronDescriptor
  , initWithCoder_device
  , neuronType
  , a
  , b
  , c
  , data_
  , initWithDeviceSelector
  , initWithDevice_neuronDescriptorSelector
  , initWithCoder_deviceSelector
  , neuronTypeSelector
  , aSelector
  , bSelector
  , cSelector
  , dataSelector

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
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNNeuron mpscnnNeuron => mpscnnNeuron -> RawId -> IO (Id MPSCNNNeuron)
initWithDevice mpscnnNeuron  device =
  sendMsg mpscnnNeuron (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Initialize the neuron filter with a neuron descriptor.
--
-- @device@ — The device the filter will run on.
--
-- @neuronDescriptor@ — The neuron descriptor.                                      For the neuron of type MPSCNNNeuronTypePReLU, the neuron                                      descriptor references an NSData object containing a float array                                      with the per feature channel value of PReLu parameter and, in this                                      case, the MPSCNNNeuron retains the NSData object.
--
-- Returns: A valid MPSCNNNeuron object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:neuronDescriptor:@
initWithDevice_neuronDescriptor :: (IsMPSCNNNeuron mpscnnNeuron, IsMPSNNNeuronDescriptor neuronDescriptor) => mpscnnNeuron -> RawId -> neuronDescriptor -> IO (Id MPSCNNNeuron)
initWithDevice_neuronDescriptor mpscnnNeuron  device neuronDescriptor =
withObjCPtr neuronDescriptor $ \raw_neuronDescriptor ->
    sendMsg mpscnnNeuron (mkSelector "initWithDevice:neuronDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_neuronDescriptor :: Ptr ())] >>= ownedObject . castPtr

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
initWithCoder_device :: (IsMPSCNNNeuron mpscnnNeuron, IsNSCoder aDecoder) => mpscnnNeuron -> aDecoder -> RawId -> IO (Id MPSCNNNeuron)
initWithCoder_device mpscnnNeuron  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpscnnNeuron (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- neuronType@
neuronType :: IsMPSCNNNeuron mpscnnNeuron => mpscnnNeuron -> IO MPSCNNNeuronType
neuronType mpscnnNeuron  =
  fmap (coerce :: CInt -> MPSCNNNeuronType) $ sendMsg mpscnnNeuron (mkSelector "neuronType") retCInt []

-- | @- a@
a :: IsMPSCNNNeuron mpscnnNeuron => mpscnnNeuron -> IO CFloat
a mpscnnNeuron  =
  sendMsg mpscnnNeuron (mkSelector "a") retCFloat []

-- | @- b@
b :: IsMPSCNNNeuron mpscnnNeuron => mpscnnNeuron -> IO CFloat
b mpscnnNeuron  =
  sendMsg mpscnnNeuron (mkSelector "b") retCFloat []

-- | @- c@
c :: IsMPSCNNNeuron mpscnnNeuron => mpscnnNeuron -> IO CFloat
c mpscnnNeuron  =
  sendMsg mpscnnNeuron (mkSelector "c") retCFloat []

-- | @- data@
data_ :: IsMPSCNNNeuron mpscnnNeuron => mpscnnNeuron -> IO (Id NSData)
data_ mpscnnNeuron  =
  sendMsg mpscnnNeuron (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:neuronDescriptor:@
initWithDevice_neuronDescriptorSelector :: Selector
initWithDevice_neuronDescriptorSelector = mkSelector "initWithDevice:neuronDescriptor:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @neuronType@
neuronTypeSelector :: Selector
neuronTypeSelector = mkSelector "neuronType"

-- | @Selector@ for @a@
aSelector :: Selector
aSelector = mkSelector "a"

-- | @Selector@ for @b@
bSelector :: Selector
bSelector = mkSelector "b"

-- | @Selector@ for @c@
cSelector :: Selector
cSelector = mkSelector "c"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

