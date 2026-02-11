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
  , initWithDeviceSelector
  , encodeGradientForDataToCommandBuffer_gradientMatrix_weightMatrix_resultGradientForDataMatrixSelector
  , encodeGradientForWeightsAndBiasToCommandBuffer_gradientMatrix_inputMatrix_resultGradientForWeightMatrix_resultGradientForBiasVectorSelector
  , initWithCoder_deviceSelector
  , copyWithZone_deviceSelector
  , sourceNumberOfFeatureVectorsSelector
  , setSourceNumberOfFeatureVectorsSelector
  , sourceOutputFeatureChannelsSelector
  , setSourceOutputFeatureChannelsSelector
  , sourceInputFeatureChannelsSelector
  , setSourceInputFeatureChannelsSelector
  , alphaSelector
  , setAlphaSelector


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

-- | @- initWithDevice:@
initWithDevice :: IsMPSMatrixFullyConnectedGradient mpsMatrixFullyConnectedGradient => mpsMatrixFullyConnectedGradient -> RawId -> IO (Id MPSMatrixFullyConnectedGradient)
initWithDevice mpsMatrixFullyConnectedGradient  device =
  sendMsg mpsMatrixFullyConnectedGradient (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
encodeGradientForDataToCommandBuffer_gradientMatrix_weightMatrix_resultGradientForDataMatrix mpsMatrixFullyConnectedGradient  commandBuffer gradientMatrix weightMatrix resultGradientForDataMatrix =
withObjCPtr gradientMatrix $ \raw_gradientMatrix ->
  withObjCPtr weightMatrix $ \raw_weightMatrix ->
    withObjCPtr resultGradientForDataMatrix $ \raw_resultGradientForDataMatrix ->
        sendMsg mpsMatrixFullyConnectedGradient (mkSelector "encodeGradientForDataToCommandBuffer:gradientMatrix:weightMatrix:resultGradientForDataMatrix:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_gradientMatrix :: Ptr ()), argPtr (castPtr raw_weightMatrix :: Ptr ()), argPtr (castPtr raw_resultGradientForDataMatrix :: Ptr ())]

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
encodeGradientForWeightsAndBiasToCommandBuffer_gradientMatrix_inputMatrix_resultGradientForWeightMatrix_resultGradientForBiasVector mpsMatrixFullyConnectedGradient  commandBuffer gradientMatrix inputMatrix resultGradientForWeightMatrix resultGradientForBiasVector =
withObjCPtr gradientMatrix $ \raw_gradientMatrix ->
  withObjCPtr inputMatrix $ \raw_inputMatrix ->
    withObjCPtr resultGradientForWeightMatrix $ \raw_resultGradientForWeightMatrix ->
      withObjCPtr resultGradientForBiasVector $ \raw_resultGradientForBiasVector ->
          sendMsg mpsMatrixFullyConnectedGradient (mkSelector "encodeGradientForWeightsAndBiasToCommandBuffer:gradientMatrix:inputMatrix:resultGradientForWeightMatrix:resultGradientForBiasVector:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_gradientMatrix :: Ptr ()), argPtr (castPtr raw_inputMatrix :: Ptr ()), argPtr (castPtr raw_resultGradientForWeightMatrix :: Ptr ()), argPtr (castPtr raw_resultGradientForBiasVector :: Ptr ())]

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
initWithCoder_device mpsMatrixFullyConnectedGradient  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsMatrixFullyConnectedGradient (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
copyWithZone_device mpsMatrixFullyConnectedGradient  zone device =
  sendMsg mpsMatrixFullyConnectedGradient (mkSelector "copyWithZone:device:") (retPtr retVoid) [argPtr zone, argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | sourceNumberOfFeatureVectors
--
-- The number of input vectors which make up the input array.              This is equivalent to the number of rows in both the input              matrix and the source gradient matrix.
--
-- This value should be equal to the corresponding value in the              forward fully connected kernel.
--
-- ObjC selector: @- sourceNumberOfFeatureVectors@
sourceNumberOfFeatureVectors :: IsMPSMatrixFullyConnectedGradient mpsMatrixFullyConnectedGradient => mpsMatrixFullyConnectedGradient -> IO CULong
sourceNumberOfFeatureVectors mpsMatrixFullyConnectedGradient  =
  sendMsg mpsMatrixFullyConnectedGradient (mkSelector "sourceNumberOfFeatureVectors") retCULong []

-- | sourceNumberOfFeatureVectors
--
-- The number of input vectors which make up the input array.              This is equivalent to the number of rows in both the input              matrix and the source gradient matrix.
--
-- This value should be equal to the corresponding value in the              forward fully connected kernel.
--
-- ObjC selector: @- setSourceNumberOfFeatureVectors:@
setSourceNumberOfFeatureVectors :: IsMPSMatrixFullyConnectedGradient mpsMatrixFullyConnectedGradient => mpsMatrixFullyConnectedGradient -> CULong -> IO ()
setSourceNumberOfFeatureVectors mpsMatrixFullyConnectedGradient  value =
  sendMsg mpsMatrixFullyConnectedGradient (mkSelector "setSourceNumberOfFeatureVectors:") retVoid [argCULong (fromIntegral value)]

-- | sourceOutputFeatureChannels
--
-- The number of feature channels in the output of the forward              fully connected layer.              This is equivalent to the number of columns in both the weight              matrix and the source gradient matrix.
--
-- This value should be equal to the corresponding value in the              forward fully connected kernel.
--
-- ObjC selector: @- sourceOutputFeatureChannels@
sourceOutputFeatureChannels :: IsMPSMatrixFullyConnectedGradient mpsMatrixFullyConnectedGradient => mpsMatrixFullyConnectedGradient -> IO CULong
sourceOutputFeatureChannels mpsMatrixFullyConnectedGradient  =
  sendMsg mpsMatrixFullyConnectedGradient (mkSelector "sourceOutputFeatureChannels") retCULong []

-- | sourceOutputFeatureChannels
--
-- The number of feature channels in the output of the forward              fully connected layer.              This is equivalent to the number of columns in both the weight              matrix and the source gradient matrix.
--
-- This value should be equal to the corresponding value in the              forward fully connected kernel.
--
-- ObjC selector: @- setSourceOutputFeatureChannels:@
setSourceOutputFeatureChannels :: IsMPSMatrixFullyConnectedGradient mpsMatrixFullyConnectedGradient => mpsMatrixFullyConnectedGradient -> CULong -> IO ()
setSourceOutputFeatureChannels mpsMatrixFullyConnectedGradient  value =
  sendMsg mpsMatrixFullyConnectedGradient (mkSelector "setSourceOutputFeatureChannels:") retVoid [argCULong (fromIntegral value)]

-- | sourceInputFeatureChannels
--
-- The number of feature channels in the input to the forward              fully connected layer.              This is equivalent to the number of columns in the input matrix.
--
-- This value should be equal to the corresponding value in the              forward fully connected kernel.
--
-- ObjC selector: @- sourceInputFeatureChannels@
sourceInputFeatureChannels :: IsMPSMatrixFullyConnectedGradient mpsMatrixFullyConnectedGradient => mpsMatrixFullyConnectedGradient -> IO CULong
sourceInputFeatureChannels mpsMatrixFullyConnectedGradient  =
  sendMsg mpsMatrixFullyConnectedGradient (mkSelector "sourceInputFeatureChannels") retCULong []

-- | sourceInputFeatureChannels
--
-- The number of feature channels in the input to the forward              fully connected layer.              This is equivalent to the number of columns in the input matrix.
--
-- This value should be equal to the corresponding value in the              forward fully connected kernel.
--
-- ObjC selector: @- setSourceInputFeatureChannels:@
setSourceInputFeatureChannels :: IsMPSMatrixFullyConnectedGradient mpsMatrixFullyConnectedGradient => mpsMatrixFullyConnectedGradient -> CULong -> IO ()
setSourceInputFeatureChannels mpsMatrixFullyConnectedGradient  value =
  sendMsg mpsMatrixFullyConnectedGradient (mkSelector "setSourceInputFeatureChannels:") retVoid [argCULong (fromIntegral value)]

-- | alpha
--
-- Scale factor to apply to the product.  This value should be equal              to the corresponding value in the forward fully connected kernel.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSMatrixFullyConnectedGradient mpsMatrixFullyConnectedGradient => mpsMatrixFullyConnectedGradient -> IO CDouble
alpha mpsMatrixFullyConnectedGradient  =
  sendMsg mpsMatrixFullyConnectedGradient (mkSelector "alpha") retCDouble []

-- | alpha
--
-- Scale factor to apply to the product.  This value should be equal              to the corresponding value in the forward fully connected kernel.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSMatrixFullyConnectedGradient mpsMatrixFullyConnectedGradient => mpsMatrixFullyConnectedGradient -> CDouble -> IO ()
setAlpha mpsMatrixFullyConnectedGradient  value =
  sendMsg mpsMatrixFullyConnectedGradient (mkSelector "setAlpha:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @encodeGradientForDataToCommandBuffer:gradientMatrix:weightMatrix:resultGradientForDataMatrix:@
encodeGradientForDataToCommandBuffer_gradientMatrix_weightMatrix_resultGradientForDataMatrixSelector :: Selector
encodeGradientForDataToCommandBuffer_gradientMatrix_weightMatrix_resultGradientForDataMatrixSelector = mkSelector "encodeGradientForDataToCommandBuffer:gradientMatrix:weightMatrix:resultGradientForDataMatrix:"

-- | @Selector@ for @encodeGradientForWeightsAndBiasToCommandBuffer:gradientMatrix:inputMatrix:resultGradientForWeightMatrix:resultGradientForBiasVector:@
encodeGradientForWeightsAndBiasToCommandBuffer_gradientMatrix_inputMatrix_resultGradientForWeightMatrix_resultGradientForBiasVectorSelector :: Selector
encodeGradientForWeightsAndBiasToCommandBuffer_gradientMatrix_inputMatrix_resultGradientForWeightMatrix_resultGradientForBiasVectorSelector = mkSelector "encodeGradientForWeightsAndBiasToCommandBuffer:gradientMatrix:inputMatrix:resultGradientForWeightMatrix:resultGradientForBiasVector:"

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

-- | @Selector@ for @sourceOutputFeatureChannels@
sourceOutputFeatureChannelsSelector :: Selector
sourceOutputFeatureChannelsSelector = mkSelector "sourceOutputFeatureChannels"

-- | @Selector@ for @setSourceOutputFeatureChannels:@
setSourceOutputFeatureChannelsSelector :: Selector
setSourceOutputFeatureChannelsSelector = mkSelector "setSourceOutputFeatureChannels:"

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

