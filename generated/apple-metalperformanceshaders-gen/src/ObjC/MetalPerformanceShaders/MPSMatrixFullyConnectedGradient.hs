{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixFullyConnectedGradient
--
-- This depends on Metal.framework.
--
-- Computes the gradient of the fully connected layer with respect              to either the weights and bias terms or the input feature vectors.
--
-- An MPSMatrixFullyConnectedGradient kernel may be used to compute              the gradients corresponding to a MPSMatrixFullyConnected kernel.              The properties, input, and weight data must match those values              used in the forward computation.              This kernel does not compute the gradient of any non-identity              activation function which may have been applied in the forward              kernel.  Such a kernel must be expressed using both MPSMatrixFullyConnected              and MPSMatrixNeuron if a gradient is to be computed.
--
-- Generated bindings for @MPSMatrixFullyConnectedGradient@.
module ObjC.MetalPerformanceShaders.MPSMatrixFullyConnectedGradient
  ( MPSMatrixFullyConnectedGradient
  , IsMPSMatrixFullyConnectedGradient(..)
  , initWithDevice
  , encodeGradientForDataToCommandBuffer_gradientMatrix_weightMatrix_resultGradientForDataMatrix
  , encodeGradientForWeightsAndBiasToCommandBuffer_gradientMatrix_inputMatrix_resultGradientForWeightMatrix_resultGradientForBiasVector
  , initWithCoder_device
  , copyWithZone_device
  , sourceNumberOfFeatureVectors
  , setSourceNumberOfFeatureVectors
  , sourceOutputFeatureChannels
  , setSourceOutputFeatureChannels
  , sourceInputFeatureChannels
  , setSourceInputFeatureChannels
  , alpha
  , setAlpha
  , alphaSelector
  , copyWithZone_deviceSelector
  , encodeGradientForDataToCommandBuffer_gradientMatrix_weightMatrix_resultGradientForDataMatrixSelector
  , encodeGradientForWeightsAndBiasToCommandBuffer_gradientMatrix_inputMatrix_resultGradientForWeightMatrix_resultGradientForBiasVectorSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , setAlphaSelector
  , setSourceInputFeatureChannelsSelector
  , setSourceNumberOfFeatureVectorsSelector
  , setSourceOutputFeatureChannelsSelector
  , sourceInputFeatureChannelsSelector
  , sourceNumberOfFeatureVectorsSelector
  , sourceOutputFeatureChannelsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:@
initWithDevice :: IsMPSMatrixFullyConnectedGradient mpsMatrixFullyConnectedGradient => mpsMatrixFullyConnectedGradient -> RawId -> IO (Id MPSMatrixFullyConnectedGradient)
initWithDevice mpsMatrixFullyConnectedGradient device =
  sendOwnedMessage mpsMatrixFullyConnectedGradient initWithDeviceSelector device

-- | Encode a MPSMatrixFullyConnectedGradient object to a command buffer and              produce the gradient of the loss function with respect to the input data.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @gradientMatrix@ — A valid MPSMatrix object which specifies the input gradient.
--
-- @weightMatrix@ — A valid MPSMatrix object which specifies the weight array.
--
-- @resultGradientForDataMatrix@ — A valid MPSMatrix object which specifies the result gradient.
--
-- This operation computes the resulting gradient of the loss function with respect              to the forward kernel's input data.  weightMatrix should contain the same values              used to compute the result of the forward kernel.
--
-- ObjC selector: @- encodeGradientForDataToCommandBuffer:gradientMatrix:weightMatrix:resultGradientForDataMatrix:@
encodeGradientForDataToCommandBuffer_gradientMatrix_weightMatrix_resultGradientForDataMatrix :: (IsMPSMatrixFullyConnectedGradient mpsMatrixFullyConnectedGradient, IsMPSMatrix resultGradientForDataMatrix) => mpsMatrixFullyConnectedGradient -> RawId -> Const (Id MPSMatrix) -> Const (Id MPSMatrix) -> resultGradientForDataMatrix -> IO ()
encodeGradientForDataToCommandBuffer_gradientMatrix_weightMatrix_resultGradientForDataMatrix mpsMatrixFullyConnectedGradient commandBuffer gradientMatrix weightMatrix resultGradientForDataMatrix =
  sendMessage mpsMatrixFullyConnectedGradient encodeGradientForDataToCommandBuffer_gradientMatrix_weightMatrix_resultGradientForDataMatrixSelector commandBuffer gradientMatrix weightMatrix (toMPSMatrix resultGradientForDataMatrix)

-- | Encode a MPSMatrixFullyConnectedGradient object to a command buffer and              produce the gradient of the loss function with respect to the weight matrix              and bias vector.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @gradientMatrix@ — A valid MPSMatrix object which specifies the input gradient.
--
-- @inputMatrix@ — A valid MPSMatrix object which specifies the input array.
--
-- @resultGradientForWeightMatrix@ — A valid MPSMatrix object which specifies the resulting gradients                                              with respect to the weights.
--
-- @resultGradientForBiasVector@ — A valid MPSVector object which specifies the resulting gradients                                              with respect to the bias terms.  If NULL these values will not be                                              returned.
--
-- This operation computes the resulting gradient of the loss function with respect              to the forward kernel's weight data.  inputMatrix should contain the same values              used to compute the result of the forward kernel.
--
-- ObjC selector: @- encodeGradientForWeightsAndBiasToCommandBuffer:gradientMatrix:inputMatrix:resultGradientForWeightMatrix:resultGradientForBiasVector:@
encodeGradientForWeightsAndBiasToCommandBuffer_gradientMatrix_inputMatrix_resultGradientForWeightMatrix_resultGradientForBiasVector :: (IsMPSMatrixFullyConnectedGradient mpsMatrixFullyConnectedGradient, IsMPSMatrix resultGradientForWeightMatrix, IsMPSVector resultGradientForBiasVector) => mpsMatrixFullyConnectedGradient -> RawId -> Const (Id MPSMatrix) -> Const (Id MPSMatrix) -> resultGradientForWeightMatrix -> resultGradientForBiasVector -> IO ()
encodeGradientForWeightsAndBiasToCommandBuffer_gradientMatrix_inputMatrix_resultGradientForWeightMatrix_resultGradientForBiasVector mpsMatrixFullyConnectedGradient commandBuffer gradientMatrix inputMatrix resultGradientForWeightMatrix resultGradientForBiasVector =
  sendMessage mpsMatrixFullyConnectedGradient encodeGradientForWeightsAndBiasToCommandBuffer_gradientMatrix_inputMatrix_resultGradientForWeightMatrix_resultGradientForBiasVectorSelector commandBuffer gradientMatrix inputMatrix (toMPSMatrix resultGradientForWeightMatrix) (toMPSVector resultGradientForBiasVector)

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSMatrixFullyConnectedGradient
--
-- @device@ — The MTLDevice on which to make the MPSMatrixFullyConnectedGradient object.
--
-- Returns: A new MPSMatrixFullyConnected object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSMatrixFullyConnectedGradient mpsMatrixFullyConnectedGradient, IsNSCoder aDecoder) => mpsMatrixFullyConnectedGradient -> aDecoder -> RawId -> IO (Id MPSMatrixFullyConnectedGradient)
initWithCoder_device mpsMatrixFullyConnectedGradient aDecoder device =
  sendOwnedMessage mpsMatrixFullyConnectedGradient initWithCoder_deviceSelector (toNSCoder aDecoder) device

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
copyWithZone_device :: IsMPSMatrixFullyConnectedGradient mpsMatrixFullyConnectedGradient => mpsMatrixFullyConnectedGradient -> Ptr () -> RawId -> IO (Id MPSMatrixFullyConnectedGradient)
copyWithZone_device mpsMatrixFullyConnectedGradient zone device =
  sendOwnedMessage mpsMatrixFullyConnectedGradient copyWithZone_deviceSelector zone device

-- | sourceNumberOfFeatureVectors
--
-- The number of input vectors which make up the input array.              This is equivalent to the number of rows in both the input              matrix and the source gradient matrix.
--
-- This value should be equal to the corresponding value in the              forward fully connected kernel.
--
-- ObjC selector: @- sourceNumberOfFeatureVectors@
sourceNumberOfFeatureVectors :: IsMPSMatrixFullyConnectedGradient mpsMatrixFullyConnectedGradient => mpsMatrixFullyConnectedGradient -> IO CULong
sourceNumberOfFeatureVectors mpsMatrixFullyConnectedGradient =
  sendMessage mpsMatrixFullyConnectedGradient sourceNumberOfFeatureVectorsSelector

-- | sourceNumberOfFeatureVectors
--
-- The number of input vectors which make up the input array.              This is equivalent to the number of rows in both the input              matrix and the source gradient matrix.
--
-- This value should be equal to the corresponding value in the              forward fully connected kernel.
--
-- ObjC selector: @- setSourceNumberOfFeatureVectors:@
setSourceNumberOfFeatureVectors :: IsMPSMatrixFullyConnectedGradient mpsMatrixFullyConnectedGradient => mpsMatrixFullyConnectedGradient -> CULong -> IO ()
setSourceNumberOfFeatureVectors mpsMatrixFullyConnectedGradient value =
  sendMessage mpsMatrixFullyConnectedGradient setSourceNumberOfFeatureVectorsSelector value

-- | sourceOutputFeatureChannels
--
-- The number of feature channels in the output of the forward              fully connected layer.              This is equivalent to the number of columns in both the weight              matrix and the source gradient matrix.
--
-- This value should be equal to the corresponding value in the              forward fully connected kernel.
--
-- ObjC selector: @- sourceOutputFeatureChannels@
sourceOutputFeatureChannels :: IsMPSMatrixFullyConnectedGradient mpsMatrixFullyConnectedGradient => mpsMatrixFullyConnectedGradient -> IO CULong
sourceOutputFeatureChannels mpsMatrixFullyConnectedGradient =
  sendMessage mpsMatrixFullyConnectedGradient sourceOutputFeatureChannelsSelector

-- | sourceOutputFeatureChannels
--
-- The number of feature channels in the output of the forward              fully connected layer.              This is equivalent to the number of columns in both the weight              matrix and the source gradient matrix.
--
-- This value should be equal to the corresponding value in the              forward fully connected kernel.
--
-- ObjC selector: @- setSourceOutputFeatureChannels:@
setSourceOutputFeatureChannels :: IsMPSMatrixFullyConnectedGradient mpsMatrixFullyConnectedGradient => mpsMatrixFullyConnectedGradient -> CULong -> IO ()
setSourceOutputFeatureChannels mpsMatrixFullyConnectedGradient value =
  sendMessage mpsMatrixFullyConnectedGradient setSourceOutputFeatureChannelsSelector value

-- | sourceInputFeatureChannels
--
-- The number of feature channels in the input to the forward              fully connected layer.              This is equivalent to the number of columns in the input matrix.
--
-- This value should be equal to the corresponding value in the              forward fully connected kernel.
--
-- ObjC selector: @- sourceInputFeatureChannels@
sourceInputFeatureChannels :: IsMPSMatrixFullyConnectedGradient mpsMatrixFullyConnectedGradient => mpsMatrixFullyConnectedGradient -> IO CULong
sourceInputFeatureChannels mpsMatrixFullyConnectedGradient =
  sendMessage mpsMatrixFullyConnectedGradient sourceInputFeatureChannelsSelector

-- | sourceInputFeatureChannels
--
-- The number of feature channels in the input to the forward              fully connected layer.              This is equivalent to the number of columns in the input matrix.
--
-- This value should be equal to the corresponding value in the              forward fully connected kernel.
--
-- ObjC selector: @- setSourceInputFeatureChannels:@
setSourceInputFeatureChannels :: IsMPSMatrixFullyConnectedGradient mpsMatrixFullyConnectedGradient => mpsMatrixFullyConnectedGradient -> CULong -> IO ()
setSourceInputFeatureChannels mpsMatrixFullyConnectedGradient value =
  sendMessage mpsMatrixFullyConnectedGradient setSourceInputFeatureChannelsSelector value

-- | alpha
--
-- Scale factor to apply to the product.  This value should be equal              to the corresponding value in the forward fully connected kernel.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSMatrixFullyConnectedGradient mpsMatrixFullyConnectedGradient => mpsMatrixFullyConnectedGradient -> IO CDouble
alpha mpsMatrixFullyConnectedGradient =
  sendMessage mpsMatrixFullyConnectedGradient alphaSelector

-- | alpha
--
-- Scale factor to apply to the product.  This value should be equal              to the corresponding value in the forward fully connected kernel.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSMatrixFullyConnectedGradient mpsMatrixFullyConnectedGradient => mpsMatrixFullyConnectedGradient -> CDouble -> IO ()
setAlpha mpsMatrixFullyConnectedGradient value =
  sendMessage mpsMatrixFullyConnectedGradient setAlphaSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSMatrixFullyConnectedGradient)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @encodeGradientForDataToCommandBuffer:gradientMatrix:weightMatrix:resultGradientForDataMatrix:@
encodeGradientForDataToCommandBuffer_gradientMatrix_weightMatrix_resultGradientForDataMatrixSelector :: Selector '[RawId, Const (Id MPSMatrix), Const (Id MPSMatrix), Id MPSMatrix] ()
encodeGradientForDataToCommandBuffer_gradientMatrix_weightMatrix_resultGradientForDataMatrixSelector = mkSelector "encodeGradientForDataToCommandBuffer:gradientMatrix:weightMatrix:resultGradientForDataMatrix:"

-- | @Selector@ for @encodeGradientForWeightsAndBiasToCommandBuffer:gradientMatrix:inputMatrix:resultGradientForWeightMatrix:resultGradientForBiasVector:@
encodeGradientForWeightsAndBiasToCommandBuffer_gradientMatrix_inputMatrix_resultGradientForWeightMatrix_resultGradientForBiasVectorSelector :: Selector '[RawId, Const (Id MPSMatrix), Const (Id MPSMatrix), Id MPSMatrix, Id MPSVector] ()
encodeGradientForWeightsAndBiasToCommandBuffer_gradientMatrix_inputMatrix_resultGradientForWeightMatrix_resultGradientForBiasVectorSelector = mkSelector "encodeGradientForWeightsAndBiasToCommandBuffer:gradientMatrix:inputMatrix:resultGradientForWeightMatrix:resultGradientForBiasVector:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSMatrixFullyConnectedGradient)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @copyWithZone:device:@
copyWithZone_deviceSelector :: Selector '[Ptr (), RawId] (Id MPSMatrixFullyConnectedGradient)
copyWithZone_deviceSelector = mkSelector "copyWithZone:device:"

-- | @Selector@ for @sourceNumberOfFeatureVectors@
sourceNumberOfFeatureVectorsSelector :: Selector '[] CULong
sourceNumberOfFeatureVectorsSelector = mkSelector "sourceNumberOfFeatureVectors"

-- | @Selector@ for @setSourceNumberOfFeatureVectors:@
setSourceNumberOfFeatureVectorsSelector :: Selector '[CULong] ()
setSourceNumberOfFeatureVectorsSelector = mkSelector "setSourceNumberOfFeatureVectors:"

-- | @Selector@ for @sourceOutputFeatureChannels@
sourceOutputFeatureChannelsSelector :: Selector '[] CULong
sourceOutputFeatureChannelsSelector = mkSelector "sourceOutputFeatureChannels"

-- | @Selector@ for @setSourceOutputFeatureChannels:@
setSourceOutputFeatureChannelsSelector :: Selector '[CULong] ()
setSourceOutputFeatureChannelsSelector = mkSelector "setSourceOutputFeatureChannels:"

-- | @Selector@ for @sourceInputFeatureChannels@
sourceInputFeatureChannelsSelector :: Selector '[] CULong
sourceInputFeatureChannelsSelector = mkSelector "sourceInputFeatureChannels"

-- | @Selector@ for @setSourceInputFeatureChannels:@
setSourceInputFeatureChannelsSelector :: Selector '[CULong] ()
setSourceInputFeatureChannelsSelector = mkSelector "setSourceInputFeatureChannels:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector '[] CDouble
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @setAlpha:@
setAlphaSelector :: Selector '[CDouble] ()
setAlphaSelector = mkSelector "setAlpha:"

