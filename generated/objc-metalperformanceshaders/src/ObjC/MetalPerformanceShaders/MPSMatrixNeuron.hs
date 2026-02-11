{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixNeuron
--
-- This depends on Metal.framework.
--
-- A neuron activation kernel that operates on matrices.
--
-- A MPSMatrixNeuron object computes:
--
-- y = neuron(alpha * x + bias)
--
-- y is the output matrix, x is the input matrix corresponding              to a collection of input vectors and bias is a vector which is broadcast              and accumulated to each row of the intermediate result.              alpha is a scale factor applied to the input.
--
-- neuron() defines the pointwise function that is applied to the intermediate result.
--
-- Note: This function computes the same result as MPSMatrixFullyConnected that has                      unit weight matrix.
--
-- Generated bindings for @MPSMatrixNeuron@.
module ObjC.MetalPerformanceShaders.MPSMatrixNeuron
  ( MPSMatrixNeuron
  , IsMPSMatrixNeuron(..)
  , setNeuronType_parameterA_parameterB_parameterC
  , neuronType
  , neuronParameterA
  , neuronParameterB
  , neuronParameterC
  , setNeuronToPReLUWithParametersA
  , initWithDevice
  , encodeToCommandBuffer_inputMatrix_biasVector_resultMatrix
  , initWithCoder_device
  , copyWithZone_device
  , sourceNumberOfFeatureVectors
  , setSourceNumberOfFeatureVectors
  , sourceInputFeatureChannels
  , setSourceInputFeatureChannels
  , alpha
  , setAlpha
  , setNeuronType_parameterA_parameterB_parameterCSelector
  , neuronTypeSelector
  , neuronParameterASelector
  , neuronParameterBSelector
  , neuronParameterCSelector
  , setNeuronToPReLUWithParametersASelector
  , initWithDeviceSelector
  , encodeToCommandBuffer_inputMatrix_biasVector_resultMatrixSelector
  , initWithCoder_deviceSelector
  , copyWithZone_deviceSelector
  , sourceNumberOfFeatureVectorsSelector
  , setSourceNumberOfFeatureVectorsSelector
  , sourceInputFeatureChannelsSelector
  , setSourceInputFeatureChannelsSelector
  , alphaSelector
  , setAlphaSelector

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

-- | Specifies a neuron activation function to be used.
--
-- This method can be used to add a neuron activation funtion of given type with              associated scalar parameters A, B, and C that are shared across all output values.              Note that this method can only be used to specify neurons which are specified by three (or fewer)              parameters shared across all output values (or channels, in CNN nomenclature). It is an error to call              this method for neuron activation functions like MPSCNNNeuronTypePReLU,              which require per-channel parameter values. For those kind of neuron activation functions,              use appropriate setter functions.  An MPSMatrixNeuron kernel is initialized              with a default neuron function of MPSCNNNeuronTypeNone.
--
-- @neuronType@ — Type of neuron activation function. For full list see MPSCNNNeuronType.h
--
-- @parameterA@ — parameterA of neuron activation that is shared across all output values.
--
-- @parameterB@ — parameterB of neuron activation that is shared across all output values.
--
-- @parameterC@ — parameterC of neuron activation that is shared across all output values.
--
-- ObjC selector: @- setNeuronType:parameterA:parameterB:parameterC:@
setNeuronType_parameterA_parameterB_parameterC :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> MPSCNNNeuronType -> CFloat -> CFloat -> CFloat -> IO ()
setNeuronType_parameterA_parameterB_parameterC mpsMatrixNeuron  neuronType parameterA parameterB parameterC =
  sendMsg mpsMatrixNeuron (mkSelector "setNeuronType:parameterA:parameterB:parameterC:") retVoid [argCInt (coerce neuronType), argCFloat (fromIntegral parameterA), argCFloat (fromIntegral parameterB), argCFloat (fromIntegral parameterC)]

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronType@
neuronType :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> IO MPSCNNNeuronType
neuronType mpsMatrixNeuron  =
  fmap (coerce :: CInt -> MPSCNNNeuronType) $ sendMsg mpsMatrixNeuron (mkSelector "neuronType") retCInt []

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterA@
neuronParameterA :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> IO CFloat
neuronParameterA mpsMatrixNeuron  =
  sendMsg mpsMatrixNeuron (mkSelector "neuronParameterA") retCFloat []

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterB@
neuronParameterB :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> IO CFloat
neuronParameterB mpsMatrixNeuron  =
  sendMsg mpsMatrixNeuron (mkSelector "neuronParameterB") retCFloat []

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterC@
neuronParameterC :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> IO CFloat
neuronParameterC mpsMatrixNeuron  =
  sendMsg mpsMatrixNeuron (mkSelector "neuronParameterC") retCFloat []

-- | Add per output value neuron parameters A for PReLu neuron activation functions.
--
-- This method sets the neuron to PReLU, zeros parameters A and B and sets the per output value              neuron parameters A to an array containing a unique value of A for each output value.
--
-- If the neuron function is f(v,a,b), it will apply
--
-- resultMatrix(i, j) = f( input(i, j), A[j], B[j] )                  where j in [0, sourceInputFeatureChannels]
--
-- See https://arxiv.org/pdf/1502.01852.pdf for details.
--
-- All other neuron types, where parameter A              and parameter B are shared across output values must be set using              -setNeuronType:parameterA:parameterB:
--
-- @A@ — An array containing float values for neuron parameter A.                      Number of entries must be equal to MIN(inputMatrix.columns - sourceMatrixOrigin.y, sourceInputFeatureChannels)
--
-- ObjC selector: @- setNeuronToPReLUWithParametersA:@
setNeuronToPReLUWithParametersA :: (IsMPSMatrixNeuron mpsMatrixNeuron, IsNSData a) => mpsMatrixNeuron -> a -> IO ()
setNeuronToPReLUWithParametersA mpsMatrixNeuron  a =
withObjCPtr a $ \raw_a ->
    sendMsg mpsMatrixNeuron (mkSelector "setNeuronToPReLUWithParametersA:") retVoid [argPtr (castPtr raw_a :: Ptr ())]

-- | @- initWithDevice:@
initWithDevice :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> RawId -> IO (Id MPSMatrixNeuron)
initWithDevice mpsMatrixNeuron  device =
  sendMsg mpsMatrixNeuron (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Encode a MPSMatrixNeuron object to a command buffer.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @inputMatrix@ — A valid MPSMatrix object which specifies the input array.
--
-- @biasVector@ — A valid MPSVector object which specifies the bias values, or                              a null object to indicate that no bias is to be applied.
--
-- @resultMatrix@ — A valid MPSMatrix object which specifies the output array.
--
-- Encodes the operation to the specified command buffer.  resultMatrix              must be large enough to hold a                  MIN(sourceNumberOfFeatureVectors, inputMatrix.rows - sourceMatrixOrigin.x)                  x                  MIN(inputMatrix.columns - sourceMatrixOrigin.y, sourceInputFeatureChannels) array.
--
-- The bias vector must contain at least                  MIN(inputMatrix.columns - sourceMatrixOrigin.y, sourceInputFeatureChannels) elements.
--
-- ObjC selector: @- encodeToCommandBuffer:inputMatrix:biasVector:resultMatrix:@
encodeToCommandBuffer_inputMatrix_biasVector_resultMatrix :: (IsMPSMatrixNeuron mpsMatrixNeuron, IsMPSMatrix inputMatrix, IsMPSVector biasVector, IsMPSMatrix resultMatrix) => mpsMatrixNeuron -> RawId -> inputMatrix -> biasVector -> resultMatrix -> IO ()
encodeToCommandBuffer_inputMatrix_biasVector_resultMatrix mpsMatrixNeuron  commandBuffer inputMatrix biasVector resultMatrix =
withObjCPtr inputMatrix $ \raw_inputMatrix ->
  withObjCPtr biasVector $ \raw_biasVector ->
    withObjCPtr resultMatrix $ \raw_resultMatrix ->
        sendMsg mpsMatrixNeuron (mkSelector "encodeToCommandBuffer:inputMatrix:biasVector:resultMatrix:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_inputMatrix :: Ptr ()), argPtr (castPtr raw_biasVector :: Ptr ()), argPtr (castPtr raw_resultMatrix :: Ptr ())]

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSMatrixNeuron
--
-- @device@ — The MTLDevice on which to make the MPSMatrixNeuron object.
--
-- Returns: A new MPSMatrixNeuron object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSMatrixNeuron mpsMatrixNeuron, IsNSCoder aDecoder) => mpsMatrixNeuron -> aDecoder -> RawId -> IO (Id MPSMatrixNeuron)
initWithCoder_device mpsMatrixNeuron  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsMatrixNeuron (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Make a copy of this kernel for a new device -
--
-- See: MPSKernel
--
-- @zone@ — The NSZone in which to allocate the object
--
-- @device@ — The device for the new MPSKernel. If nil, then use                          self.device.
--
-- Returns: A pointer to a copy of this MPSKernel. This will fail, returning              nil if the device is not supported. Devices must be              MTLFeatureSet_iOS_GPUFamily2_v1 or later.
--
-- ObjC selector: @- copyWithZone:device:@
copyWithZone_device :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> Ptr () -> RawId -> IO (Id MPSMatrixNeuron)
copyWithZone_device mpsMatrixNeuron  zone device =
  sendMsg mpsMatrixNeuron (mkSelector "copyWithZone:device:") (retPtr retVoid) [argPtr zone, argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | sourceNumberOfFeatureVectors
--
-- The number of input vectors which make up the input array.  This              is equivalent to the number of rows to consider from the primary              source matrix.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available number of inputs is              used.  The value of NSUIntegerMax thus indicates that all available input              rows (beginning at sourceMatrixOrigin.x) should be considered.
--
-- ObjC selector: @- sourceNumberOfFeatureVectors@
sourceNumberOfFeatureVectors :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> IO CULong
sourceNumberOfFeatureVectors mpsMatrixNeuron  =
  sendMsg mpsMatrixNeuron (mkSelector "sourceNumberOfFeatureVectors") retCULong []

-- | sourceNumberOfFeatureVectors
--
-- The number of input vectors which make up the input array.  This              is equivalent to the number of rows to consider from the primary              source matrix.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available number of inputs is              used.  The value of NSUIntegerMax thus indicates that all available input              rows (beginning at sourceMatrixOrigin.x) should be considered.
--
-- ObjC selector: @- setSourceNumberOfFeatureVectors:@
setSourceNumberOfFeatureVectors :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> CULong -> IO ()
setSourceNumberOfFeatureVectors mpsMatrixNeuron  value =
  sendMsg mpsMatrixNeuron (mkSelector "setSourceNumberOfFeatureVectors:") retVoid [argCULong (fromIntegral value)]

-- | sourceInputFeatureChannels
--
-- The input size to to use in the operation.  This is equivalent to the              number of columns in the primary (input array) source matrix to consider              and the number of channels to produce for the output matrix.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available input size is used.              The value of NSUIntegerMax thus indicates that all available columns in              the input array (beginning at sourceMatrixOrigin.y) should be considered.              Defines also the number of output feature channels.              Note: The value used in the operation will be              MIN(inputMatrix.columns - sourceMatrixOrigin.y, sourceInputFeatureChannels)
--
-- ObjC selector: @- sourceInputFeatureChannels@
sourceInputFeatureChannels :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> IO CULong
sourceInputFeatureChannels mpsMatrixNeuron  =
  sendMsg mpsMatrixNeuron (mkSelector "sourceInputFeatureChannels") retCULong []

-- | sourceInputFeatureChannels
--
-- The input size to to use in the operation.  This is equivalent to the              number of columns in the primary (input array) source matrix to consider              and the number of channels to produce for the output matrix.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available input size is used.              The value of NSUIntegerMax thus indicates that all available columns in              the input array (beginning at sourceMatrixOrigin.y) should be considered.              Defines also the number of output feature channels.              Note: The value used in the operation will be              MIN(inputMatrix.columns - sourceMatrixOrigin.y, sourceInputFeatureChannels)
--
-- ObjC selector: @- setSourceInputFeatureChannels:@
setSourceInputFeatureChannels :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> CULong -> IO ()
setSourceInputFeatureChannels mpsMatrixNeuron  value =
  sendMsg mpsMatrixNeuron (mkSelector "setSourceInputFeatureChannels:") retVoid [argCULong (fromIntegral value)]

-- | alpha
--
-- The scale factor to apply to the input.  Specified in double              precision.  Will be converted to the appropriate precision in the              implementation subject to rounding and/or clamping as necessary.              Defaults to 1.0 at initialization time.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> IO CDouble
alpha mpsMatrixNeuron  =
  sendMsg mpsMatrixNeuron (mkSelector "alpha") retCDouble []

-- | alpha
--
-- The scale factor to apply to the input.  Specified in double              precision.  Will be converted to the appropriate precision in the              implementation subject to rounding and/or clamping as necessary.              Defaults to 1.0 at initialization time.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> CDouble -> IO ()
setAlpha mpsMatrixNeuron  value =
  sendMsg mpsMatrixNeuron (mkSelector "setAlpha:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setNeuronType:parameterA:parameterB:parameterC:@
setNeuronType_parameterA_parameterB_parameterCSelector :: Selector
setNeuronType_parameterA_parameterB_parameterCSelector = mkSelector "setNeuronType:parameterA:parameterB:parameterC:"

-- | @Selector@ for @neuronType@
neuronTypeSelector :: Selector
neuronTypeSelector = mkSelector "neuronType"

-- | @Selector@ for @neuronParameterA@
neuronParameterASelector :: Selector
neuronParameterASelector = mkSelector "neuronParameterA"

-- | @Selector@ for @neuronParameterB@
neuronParameterBSelector :: Selector
neuronParameterBSelector = mkSelector "neuronParameterB"

-- | @Selector@ for @neuronParameterC@
neuronParameterCSelector :: Selector
neuronParameterCSelector = mkSelector "neuronParameterC"

-- | @Selector@ for @setNeuronToPReLUWithParametersA:@
setNeuronToPReLUWithParametersASelector :: Selector
setNeuronToPReLUWithParametersASelector = mkSelector "setNeuronToPReLUWithParametersA:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @encodeToCommandBuffer:inputMatrix:biasVector:resultMatrix:@
encodeToCommandBuffer_inputMatrix_biasVector_resultMatrixSelector :: Selector
encodeToCommandBuffer_inputMatrix_biasVector_resultMatrixSelector = mkSelector "encodeToCommandBuffer:inputMatrix:biasVector:resultMatrix:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @copyWithZone:device:@
copyWithZone_deviceSelector :: Selector
copyWithZone_deviceSelector = mkSelector "copyWithZone:device:"

-- | @Selector@ for @sourceNumberOfFeatureVectors@
sourceNumberOfFeatureVectorsSelector :: Selector
sourceNumberOfFeatureVectorsSelector = mkSelector "sourceNumberOfFeatureVectors"

-- | @Selector@ for @setSourceNumberOfFeatureVectors:@
setSourceNumberOfFeatureVectorsSelector :: Selector
setSourceNumberOfFeatureVectorsSelector = mkSelector "setSourceNumberOfFeatureVectors:"

-- | @Selector@ for @sourceInputFeatureChannels@
sourceInputFeatureChannelsSelector :: Selector
sourceInputFeatureChannelsSelector = mkSelector "sourceInputFeatureChannels"

-- | @Selector@ for @setSourceInputFeatureChannels:@
setSourceInputFeatureChannelsSelector :: Selector
setSourceInputFeatureChannelsSelector = mkSelector "setSourceInputFeatureChannels:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @setAlpha:@
setAlphaSelector :: Selector
setAlphaSelector = mkSelector "setAlpha:"

