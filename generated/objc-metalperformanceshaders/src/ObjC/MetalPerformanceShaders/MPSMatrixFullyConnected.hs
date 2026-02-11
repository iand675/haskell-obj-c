{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixFullyConnected
--
-- This depends on Metal.framework.
--
-- Applies a fully connected neural network layer by performing a              a matrix multiplication, adding a bias vector, scaling, and applying a              neuron activation function.
--
-- A MPSMatrixFullyConnected object computes:
--
-- y = neuron(alpha * x * W + bias)
--
-- y is the output matrix, x and W are input matrices corresponding              to a collection of input vectors and weights respectively, and bias              is a vector which is broadcast and accumulated to each row              of the product.  alpha is a scale factor applied to the product.
--
-- neuron() is a pointwise function applied to the intermediate result.
--
-- Generated bindings for @MPSMatrixFullyConnected@.
module ObjC.MetalPerformanceShaders.MPSMatrixFullyConnected
  ( MPSMatrixFullyConnected
  , IsMPSMatrixFullyConnected(..)
  , setNeuronType_parameterA_parameterB_parameterC
  , neuronType
  , neuronParameterA
  , neuronParameterB
  , neuronParameterC
  , initWithDevice
  , encodeToCommandBuffer_inputMatrix_weightMatrix_biasVector_resultMatrix
  , initWithCoder_device
  , copyWithZone_device
  , sourceNumberOfFeatureVectors
  , setSourceNumberOfFeatureVectors
  , sourceInputFeatureChannels
  , setSourceInputFeatureChannels
  , sourceOutputFeatureChannels
  , setSourceOutputFeatureChannels
  , alpha
  , setAlpha
  , setNeuronType_parameterA_parameterB_parameterCSelector
  , neuronTypeSelector
  , neuronParameterASelector
  , neuronParameterBSelector
  , neuronParameterCSelector
  , initWithDeviceSelector
  , encodeToCommandBuffer_inputMatrix_weightMatrix_biasVector_resultMatrixSelector
  , initWithCoder_deviceSelector
  , copyWithZone_deviceSelector
  , sourceNumberOfFeatureVectorsSelector
  , setSourceNumberOfFeatureVectorsSelector
  , sourceInputFeatureChannelsSelector
  , setSourceInputFeatureChannelsSelector
  , sourceOutputFeatureChannelsSelector
  , setSourceOutputFeatureChannelsSelector
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
-- This method can be used to add a neuron activation funtion of given type with              associated scalar parameters A, B, and C that are shared across all output values.              Note that this method can only be used to specify neurons which are specified by three (or fewer)              parameters shared across all output values (or channels, in CNN nomenclature). It is an error to call              this method for neuron activation functions like MPSCNNNeuronTypePReLU,              which require per-channel parameter values. For those kind of neuron activation functions,              use appropriate setter functions.  An MPSMatrixFullyConnected kernel is initialized              with a default neuron function of MPSCNNNeuronTypeNone.
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
setNeuronType_parameterA_parameterB_parameterC :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> MPSCNNNeuronType -> CFloat -> CFloat -> CFloat -> IO ()
setNeuronType_parameterA_parameterB_parameterC mpsMatrixFullyConnected  neuronType parameterA parameterB parameterC =
  sendMsg mpsMatrixFullyConnected (mkSelector "setNeuronType:parameterA:parameterB:parameterC:") retVoid [argCInt (coerce neuronType), argCFloat (fromIntegral parameterA), argCFloat (fromIntegral parameterB), argCFloat (fromIntegral parameterC)]

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronType@
neuronType :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> IO MPSCNNNeuronType
neuronType mpsMatrixFullyConnected  =
  fmap (coerce :: CInt -> MPSCNNNeuronType) $ sendMsg mpsMatrixFullyConnected (mkSelector "neuronType") retCInt []

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterA@
neuronParameterA :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> IO CFloat
neuronParameterA mpsMatrixFullyConnected  =
  sendMsg mpsMatrixFullyConnected (mkSelector "neuronParameterA") retCFloat []

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterB@
neuronParameterB :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> IO CFloat
neuronParameterB mpsMatrixFullyConnected  =
  sendMsg mpsMatrixFullyConnected (mkSelector "neuronParameterB") retCFloat []

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterC@
neuronParameterC :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> IO CFloat
neuronParameterC mpsMatrixFullyConnected  =
  sendMsg mpsMatrixFullyConnected (mkSelector "neuronParameterC") retCFloat []

-- | @- initWithDevice:@
initWithDevice :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> RawId -> IO (Id MPSMatrixFullyConnected)
initWithDevice mpsMatrixFullyConnected  device =
  sendMsg mpsMatrixFullyConnected (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Encode a MPSMatrixFullyConnected object to a command buffer.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @inputMatrix@ — A valid MPSMatrix object which specifies the input array.
--
-- @weightMatrix@ — A valid MPSMatrix object which specifies the weight array.
--
-- @biasVector@ — A valid MPSVector object which specifies the bias values, or                              a null object to indicate that no bias is to be applied.
--
-- @resultMatrix@ — A valid MPSMatrix object which specifies the output array.
--
-- Encodes the operation to the specified command buffer.  resultMatrix              must be large enough to hold a                  MIN(sourceNumberOfInputs,                      inputMatrix.rows - primarySourceMatrixOrigin.x)                  x                  MIN(sourceOutputFeatureChannels,                      weightMatrix.columns - secondarySourceMatrixOrigin.y) array.
--
-- The bias vector must contain at least                  MIN(sourceOutputFeatureChannels, weightMatrix.columns - secondarySourceMatrixOrigin.y) elements.
--
-- ObjC selector: @- encodeToCommandBuffer:inputMatrix:weightMatrix:biasVector:resultMatrix:@
encodeToCommandBuffer_inputMatrix_weightMatrix_biasVector_resultMatrix :: (IsMPSMatrixFullyConnected mpsMatrixFullyConnected, IsMPSMatrix inputMatrix, IsMPSMatrix weightMatrix, IsMPSVector biasVector, IsMPSMatrix resultMatrix) => mpsMatrixFullyConnected -> RawId -> inputMatrix -> weightMatrix -> biasVector -> resultMatrix -> IO ()
encodeToCommandBuffer_inputMatrix_weightMatrix_biasVector_resultMatrix mpsMatrixFullyConnected  commandBuffer inputMatrix weightMatrix biasVector resultMatrix =
withObjCPtr inputMatrix $ \raw_inputMatrix ->
  withObjCPtr weightMatrix $ \raw_weightMatrix ->
    withObjCPtr biasVector $ \raw_biasVector ->
      withObjCPtr resultMatrix $ \raw_resultMatrix ->
          sendMsg mpsMatrixFullyConnected (mkSelector "encodeToCommandBuffer:inputMatrix:weightMatrix:biasVector:resultMatrix:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_inputMatrix :: Ptr ()), argPtr (castPtr raw_weightMatrix :: Ptr ()), argPtr (castPtr raw_biasVector :: Ptr ()), argPtr (castPtr raw_resultMatrix :: Ptr ())]

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSMatrixFullyConnected
--
-- @device@ — The MTLDevice on which to make the MPSMatrixFullyConnected object.
--
-- Returns: A new MPSMatrixFullyConnected object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSMatrixFullyConnected mpsMatrixFullyConnected, IsNSCoder aDecoder) => mpsMatrixFullyConnected -> aDecoder -> RawId -> IO (Id MPSMatrixFullyConnected)
initWithCoder_device mpsMatrixFullyConnected  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsMatrixFullyConnected (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
copyWithZone_device :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> Ptr () -> RawId -> IO (Id MPSMatrixFullyConnected)
copyWithZone_device mpsMatrixFullyConnected  zone device =
  sendMsg mpsMatrixFullyConnected (mkSelector "copyWithZone:device:") (retPtr retVoid) [argPtr zone, argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | sourceNumberOfFeatureVectors
--
-- The number of input vectors which make up the input array.  This              is equivalent to the number of rows to consider from the primary              source matrix.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available number of inputs is              used.  The value of NSUIntegerMax thus indicates that all available input              rows (beginning at primarySourceMatrixOrigin.x) should be considered.
--
-- ObjC selector: @- sourceNumberOfFeatureVectors@
sourceNumberOfFeatureVectors :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> IO CULong
sourceNumberOfFeatureVectors mpsMatrixFullyConnected  =
  sendMsg mpsMatrixFullyConnected (mkSelector "sourceNumberOfFeatureVectors") retCULong []

-- | sourceNumberOfFeatureVectors
--
-- The number of input vectors which make up the input array.  This              is equivalent to the number of rows to consider from the primary              source matrix.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available number of inputs is              used.  The value of NSUIntegerMax thus indicates that all available input              rows (beginning at primarySourceMatrixOrigin.x) should be considered.
--
-- ObjC selector: @- setSourceNumberOfFeatureVectors:@
setSourceNumberOfFeatureVectors :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> CULong -> IO ()
setSourceNumberOfFeatureVectors mpsMatrixFullyConnected  value =
  sendMsg mpsMatrixFullyConnected (mkSelector "setSourceNumberOfFeatureVectors:") retVoid [argCULong (fromIntegral value)]

-- | sourceInputFeatureChannels
--
-- The input size to to use in the operation.  This is equivalent to the              number of columns and the number of rows in the primary (input array) and              secondary (weight array) source matrices respectively.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available input size is used.              The value of NSUIntegerMax thus indicates that all available              columns in the input array (beginning at primarySourceMatrixOrigin.y) and all              available rows in the weight array (beginning at secondarySourceMatrixOrigin.x)              should be considered.              Note: The value used in the operation will be              MIN(MIN(inputMatrix.columns - primarySourceMatrixOrigin.y,                      weightMatrix.rows - secondarySourceMatrixOrigin.x),                  sourceInputFeatureChannels)
--
-- ObjC selector: @- sourceInputFeatureChannels@
sourceInputFeatureChannels :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> IO CULong
sourceInputFeatureChannels mpsMatrixFullyConnected  =
  sendMsg mpsMatrixFullyConnected (mkSelector "sourceInputFeatureChannels") retCULong []

-- | sourceInputFeatureChannels
--
-- The input size to to use in the operation.  This is equivalent to the              number of columns and the number of rows in the primary (input array) and              secondary (weight array) source matrices respectively.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available input size is used.              The value of NSUIntegerMax thus indicates that all available              columns in the input array (beginning at primarySourceMatrixOrigin.y) and all              available rows in the weight array (beginning at secondarySourceMatrixOrigin.x)              should be considered.              Note: The value used in the operation will be              MIN(MIN(inputMatrix.columns - primarySourceMatrixOrigin.y,                      weightMatrix.rows - secondarySourceMatrixOrigin.x),                  sourceInputFeatureChannels)
--
-- ObjC selector: @- setSourceInputFeatureChannels:@
setSourceInputFeatureChannels :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> CULong -> IO ()
setSourceInputFeatureChannels mpsMatrixFullyConnected  value =
  sendMsg mpsMatrixFullyConnected (mkSelector "setSourceInputFeatureChannels:") retVoid [argCULong (fromIntegral value)]

-- | sourceOutputFeatureChannels
--
-- The output size to to use in the operation.  This is equivalent to the              number of columns to consider in the weight array, or the secondary source matrix.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available output size is used.              The value of NSUIntegerMax thus indicates that all available              columns in the weight array (beginning at secondarySourceMatrixOrigin.y)              should be considered.
--
-- ObjC selector: @- sourceOutputFeatureChannels@
sourceOutputFeatureChannels :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> IO CULong
sourceOutputFeatureChannels mpsMatrixFullyConnected  =
  sendMsg mpsMatrixFullyConnected (mkSelector "sourceOutputFeatureChannels") retCULong []

-- | sourceOutputFeatureChannels
--
-- The output size to to use in the operation.  This is equivalent to the              number of columns to consider in the weight array, or the secondary source matrix.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available output size is used.              The value of NSUIntegerMax thus indicates that all available              columns in the weight array (beginning at secondarySourceMatrixOrigin.y)              should be considered.
--
-- ObjC selector: @- setSourceOutputFeatureChannels:@
setSourceOutputFeatureChannels :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> CULong -> IO ()
setSourceOutputFeatureChannels mpsMatrixFullyConnected  value =
  sendMsg mpsMatrixFullyConnected (mkSelector "setSourceOutputFeatureChannels:") retVoid [argCULong (fromIntegral value)]

-- | alpha
--
-- The scale factor to apply to the product.  Specified in double              precision.  Will be converted to the appropriate precision in the              implementation subject to rounding and/or clamping as necessary.              Defaults to 1.0 at initialization time.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> IO CDouble
alpha mpsMatrixFullyConnected  =
  sendMsg mpsMatrixFullyConnected (mkSelector "alpha") retCDouble []

-- | alpha
--
-- The scale factor to apply to the product.  Specified in double              precision.  Will be converted to the appropriate precision in the              implementation subject to rounding and/or clamping as necessary.              Defaults to 1.0 at initialization time.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> CDouble -> IO ()
setAlpha mpsMatrixFullyConnected  value =
  sendMsg mpsMatrixFullyConnected (mkSelector "setAlpha:") retVoid [argCDouble (fromIntegral value)]

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

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @encodeToCommandBuffer:inputMatrix:weightMatrix:biasVector:resultMatrix:@
encodeToCommandBuffer_inputMatrix_weightMatrix_biasVector_resultMatrixSelector :: Selector
encodeToCommandBuffer_inputMatrix_weightMatrix_biasVector_resultMatrixSelector = mkSelector "encodeToCommandBuffer:inputMatrix:weightMatrix:biasVector:resultMatrix:"

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

-- | @Selector@ for @sourceOutputFeatureChannels@
sourceOutputFeatureChannelsSelector :: Selector
sourceOutputFeatureChannelsSelector = mkSelector "sourceOutputFeatureChannels"

-- | @Selector@ for @setSourceOutputFeatureChannels:@
setSourceOutputFeatureChannelsSelector :: Selector
setSourceOutputFeatureChannelsSelector = mkSelector "setSourceOutputFeatureChannels:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @setAlpha:@
setAlphaSelector :: Selector
setAlphaSelector = mkSelector "setAlpha:"

