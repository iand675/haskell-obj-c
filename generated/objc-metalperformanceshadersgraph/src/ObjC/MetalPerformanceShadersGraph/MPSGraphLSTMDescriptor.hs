{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The class that defines the parameters for a long short-term memory (LSTM) operation.
--
-- Use this descriptor with the following ``MPSGraph`` methods: - ``MPSGraph/LSTMWithSourceTensor:recurrentWeight:initState:initCell:descriptor:name:`` - ``MPSGraph/LSTMWithSourceTensor:recurrentWeight:inputWeight:bias:initState:initCell:descriptor:name:`` - ``MPSGraph/LSTMWithSourceTensor:recurrentWeight:inputWeight:bias:initState:initCell:mask:peephole:descriptor:name:`` - ``MPSGraph/LSTMGradientsWithSourceTensor:recurrentWeight:sourceGradient:zState:cellOutputFwd:descriptor:name:`` - ``MPSGraph/LSTMGradientsWithSourceTensor:recurrentWeight:sourceGradient:zState:cellOutputFwd:inputWeight:bias:initState:initCell:descriptor:name:`` - ``MPSGraph/LSTMGradientsWithSourceTensor:recurrentWeight:sourceGradient:zState:cellOutputFwd:inputWeight:bias:initState:initCell:mask:descriptor:name:`` - ``MPSGraph/LSTMGradientsWithSourceTensor:recurrentWeight:sourceGradient:zState:cellOutputFwd:stateGradient:cellGradient:inputWeight:bias:initState:initCell:mask:peephole:descriptor:name:``
--
-- Generated bindings for @MPSGraphLSTMDescriptor@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphLSTMDescriptor
  ( MPSGraphLSTMDescriptor
  , IsMPSGraphLSTMDescriptor(..)
  , descriptor
  , reverse_
  , setReverse
  , bidirectional
  , setBidirectional
  , produceCell
  , setProduceCell
  , training
  , setTraining
  , forgetGateLast
  , setForgetGateLast
  , inputGateActivation
  , setInputGateActivation
  , forgetGateActivation
  , setForgetGateActivation
  , cellGateActivation
  , setCellGateActivation
  , outputGateActivation
  , setOutputGateActivation
  , activation
  , setActivation
  , descriptorSelector
  , reverseSelector
  , setReverseSelector
  , bidirectionalSelector
  , setBidirectionalSelector
  , produceCellSelector
  , setProduceCellSelector
  , trainingSelector
  , setTrainingSelector
  , forgetGateLastSelector
  , setForgetGateLastSelector
  , inputGateActivationSelector
  , setInputGateActivationSelector
  , forgetGateActivationSelector
  , setForgetGateActivationSelector
  , cellGateActivationSelector
  , setCellGateActivationSelector
  , outputGateActivationSelector
  , setOutputGateActivationSelector
  , activationSelector
  , setActivationSelector

  -- * Enum types
  , MPSGraphRNNActivation(MPSGraphRNNActivation)
  , pattern MPSGraphRNNActivationNone
  , pattern MPSGraphRNNActivationRelu
  , pattern MPSGraphRNNActivationTanh
  , pattern MPSGraphRNNActivationSigmoid
  , pattern MPSGraphRNNActivationHardSigmoid

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

import ObjC.MetalPerformanceShadersGraph.Internal.Classes
import ObjC.MetalPerformanceShadersGraph.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates an LSTM descriptor with default values.
--
-- ObjC selector: @+ descriptor@
descriptor :: IO (Id MPSGraphLSTMDescriptor)
descriptor  =
  do
    cls' <- getRequiredClass "MPSGraphLSTMDescriptor"
    sendClassMsg cls' (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A parameter that defines time direction of the input sequence.
--
-- If set to @YES@ then the input sequence is passed in reverse time order to the layer. Note: Ignored when @bidirectional = YES@. Default value: @NO@.
--
-- ObjC selector: @- reverse@
reverse_ :: IsMPSGraphLSTMDescriptor mpsGraphLSTMDescriptor => mpsGraphLSTMDescriptor -> IO Bool
reverse_ mpsGraphLSTMDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsGraphLSTMDescriptor (mkSelector "reverse") retCULong []

-- | A parameter that defines time direction of the input sequence.
--
-- If set to @YES@ then the input sequence is passed in reverse time order to the layer. Note: Ignored when @bidirectional = YES@. Default value: @NO@.
--
-- ObjC selector: @- setReverse:@
setReverse :: IsMPSGraphLSTMDescriptor mpsGraphLSTMDescriptor => mpsGraphLSTMDescriptor -> Bool -> IO ()
setReverse mpsGraphLSTMDescriptor  value =
  sendMsg mpsGraphLSTMDescriptor (mkSelector "setReverse:") retVoid [argCULong (if value then 1 else 0)]

-- | A parameter that defines a bidirectional LSTM layer.
--
-- If set to @YES@ then the input sequence is traversed in both directions and the two results are concatenated together on the channel-axis. Default value: @NO@.
--
-- ObjC selector: @- bidirectional@
bidirectional :: IsMPSGraphLSTMDescriptor mpsGraphLSTMDescriptor => mpsGraphLSTMDescriptor -> IO Bool
bidirectional mpsGraphLSTMDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsGraphLSTMDescriptor (mkSelector "bidirectional") retCULong []

-- | A parameter that defines a bidirectional LSTM layer.
--
-- If set to @YES@ then the input sequence is traversed in both directions and the two results are concatenated together on the channel-axis. Default value: @NO@.
--
-- ObjC selector: @- setBidirectional:@
setBidirectional :: IsMPSGraphLSTMDescriptor mpsGraphLSTMDescriptor => mpsGraphLSTMDescriptor -> Bool -> IO ()
setBidirectional mpsGraphLSTMDescriptor  value =
  sendMsg mpsGraphLSTMDescriptor (mkSelector "setBidirectional:") retVoid [argCULong (if value then 1 else 0)]

-- | A parameter that controls whether or not to return the output cell from the LSTM layer.
--
-- If set to @YES@ then this layer will produce the internal cell of the LSTM unit as secondary output. Default value: @NO@.
--
-- ObjC selector: @- produceCell@
produceCell :: IsMPSGraphLSTMDescriptor mpsGraphLSTMDescriptor => mpsGraphLSTMDescriptor -> IO Bool
produceCell mpsGraphLSTMDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsGraphLSTMDescriptor (mkSelector "produceCell") retCULong []

-- | A parameter that controls whether or not to return the output cell from the LSTM layer.
--
-- If set to @YES@ then this layer will produce the internal cell of the LSTM unit as secondary output. Default value: @NO@.
--
-- ObjC selector: @- setProduceCell:@
setProduceCell :: IsMPSGraphLSTMDescriptor mpsGraphLSTMDescriptor => mpsGraphLSTMDescriptor -> Bool -> IO ()
setProduceCell mpsGraphLSTMDescriptor  value =
  sendMsg mpsGraphLSTMDescriptor (mkSelector "setProduceCell:") retVoid [argCULong (if value then 1 else 0)]

-- | A parameter that enables the LSTM layer to support training.
--
-- If set to @YES@ then the layer will produce training state tensor as a secondary output. Default value: @NO@.
--
-- ObjC selector: @- training@
training :: IsMPSGraphLSTMDescriptor mpsGraphLSTMDescriptor => mpsGraphLSTMDescriptor -> IO Bool
training mpsGraphLSTMDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsGraphLSTMDescriptor (mkSelector "training") retCULong []

-- | A parameter that enables the LSTM layer to support training.
--
-- If set to @YES@ then the layer will produce training state tensor as a secondary output. Default value: @NO@.
--
-- ObjC selector: @- setTraining:@
setTraining :: IsMPSGraphLSTMDescriptor mpsGraphLSTMDescriptor => mpsGraphLSTMDescriptor -> Bool -> IO ()
setTraining mpsGraphLSTMDescriptor  value =
  sendMsg mpsGraphLSTMDescriptor (mkSelector "setTraining:") retVoid [argCULong (if value then 1 else 0)]

-- | A parameter that controls the internal order of the LSTM gates.
--
-- If set to @YES@ then the layer will use the gate-ordering @[ i, z, f, o ]@ instead of default @[ i, f, z, o ]@. Default value: @NO@
--
-- ObjC selector: @- forgetGateLast@
forgetGateLast :: IsMPSGraphLSTMDescriptor mpsGraphLSTMDescriptor => mpsGraphLSTMDescriptor -> IO Bool
forgetGateLast mpsGraphLSTMDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsGraphLSTMDescriptor (mkSelector "forgetGateLast") retCULong []

-- | A parameter that controls the internal order of the LSTM gates.
--
-- If set to @YES@ then the layer will use the gate-ordering @[ i, z, f, o ]@ instead of default @[ i, f, z, o ]@. Default value: @NO@
--
-- ObjC selector: @- setForgetGateLast:@
setForgetGateLast :: IsMPSGraphLSTMDescriptor mpsGraphLSTMDescriptor => mpsGraphLSTMDescriptor -> Bool -> IO ()
setForgetGateLast mpsGraphLSTMDescriptor  value =
  sendMsg mpsGraphLSTMDescriptor (mkSelector "setForgetGateLast:") retVoid [argCULong (if value then 1 else 0)]

-- | A parameter that defines the activation function used with the input gate of the LSTM operation.
--
-- Default value: @MPSGraphRNNActivationSigmoid@.
--
-- ObjC selector: @- inputGateActivation@
inputGateActivation :: IsMPSGraphLSTMDescriptor mpsGraphLSTMDescriptor => mpsGraphLSTMDescriptor -> IO MPSGraphRNNActivation
inputGateActivation mpsGraphLSTMDescriptor  =
  fmap (coerce :: CULong -> MPSGraphRNNActivation) $ sendMsg mpsGraphLSTMDescriptor (mkSelector "inputGateActivation") retCULong []

-- | A parameter that defines the activation function used with the input gate of the LSTM operation.
--
-- Default value: @MPSGraphRNNActivationSigmoid@.
--
-- ObjC selector: @- setInputGateActivation:@
setInputGateActivation :: IsMPSGraphLSTMDescriptor mpsGraphLSTMDescriptor => mpsGraphLSTMDescriptor -> MPSGraphRNNActivation -> IO ()
setInputGateActivation mpsGraphLSTMDescriptor  value =
  sendMsg mpsGraphLSTMDescriptor (mkSelector "setInputGateActivation:") retVoid [argCULong (coerce value)]

-- | A parameter that defines the activation function used with the forget gate of the LSTM operation.
--
-- Default value: @MPSGraphRNNActivationSigmoid@.
--
-- ObjC selector: @- forgetGateActivation@
forgetGateActivation :: IsMPSGraphLSTMDescriptor mpsGraphLSTMDescriptor => mpsGraphLSTMDescriptor -> IO MPSGraphRNNActivation
forgetGateActivation mpsGraphLSTMDescriptor  =
  fmap (coerce :: CULong -> MPSGraphRNNActivation) $ sendMsg mpsGraphLSTMDescriptor (mkSelector "forgetGateActivation") retCULong []

-- | A parameter that defines the activation function used with the forget gate of the LSTM operation.
--
-- Default value: @MPSGraphRNNActivationSigmoid@.
--
-- ObjC selector: @- setForgetGateActivation:@
setForgetGateActivation :: IsMPSGraphLSTMDescriptor mpsGraphLSTMDescriptor => mpsGraphLSTMDescriptor -> MPSGraphRNNActivation -> IO ()
setForgetGateActivation mpsGraphLSTMDescriptor  value =
  sendMsg mpsGraphLSTMDescriptor (mkSelector "setForgetGateActivation:") retVoid [argCULong (coerce value)]

-- | A parameter that defines the activation function used with the cell gate of the LSTM operation.
--
-- Default value: @MPSGraphRNNActivationTanh@.
--
-- ObjC selector: @- cellGateActivation@
cellGateActivation :: IsMPSGraphLSTMDescriptor mpsGraphLSTMDescriptor => mpsGraphLSTMDescriptor -> IO MPSGraphRNNActivation
cellGateActivation mpsGraphLSTMDescriptor  =
  fmap (coerce :: CULong -> MPSGraphRNNActivation) $ sendMsg mpsGraphLSTMDescriptor (mkSelector "cellGateActivation") retCULong []

-- | A parameter that defines the activation function used with the cell gate of the LSTM operation.
--
-- Default value: @MPSGraphRNNActivationTanh@.
--
-- ObjC selector: @- setCellGateActivation:@
setCellGateActivation :: IsMPSGraphLSTMDescriptor mpsGraphLSTMDescriptor => mpsGraphLSTMDescriptor -> MPSGraphRNNActivation -> IO ()
setCellGateActivation mpsGraphLSTMDescriptor  value =
  sendMsg mpsGraphLSTMDescriptor (mkSelector "setCellGateActivation:") retVoid [argCULong (coerce value)]

-- | A parameter that defines the activation function used with the output gate of the LSTM operation.
--
-- Default value: @MPSGraphRNNActivationSigmoid@.
--
-- ObjC selector: @- outputGateActivation@
outputGateActivation :: IsMPSGraphLSTMDescriptor mpsGraphLSTMDescriptor => mpsGraphLSTMDescriptor -> IO MPSGraphRNNActivation
outputGateActivation mpsGraphLSTMDescriptor  =
  fmap (coerce :: CULong -> MPSGraphRNNActivation) $ sendMsg mpsGraphLSTMDescriptor (mkSelector "outputGateActivation") retCULong []

-- | A parameter that defines the activation function used with the output gate of the LSTM operation.
--
-- Default value: @MPSGraphRNNActivationSigmoid@.
--
-- ObjC selector: @- setOutputGateActivation:@
setOutputGateActivation :: IsMPSGraphLSTMDescriptor mpsGraphLSTMDescriptor => mpsGraphLSTMDescriptor -> MPSGraphRNNActivation -> IO ()
setOutputGateActivation mpsGraphLSTMDescriptor  value =
  sendMsg mpsGraphLSTMDescriptor (mkSelector "setOutputGateActivation:") retVoid [argCULong (coerce value)]

-- | A parameter that defines the activation function used with the current cell value of the LSTM operation.
--
-- Default value: @MPSGraphRNNActivationTanh@.
--
-- ObjC selector: @- activation@
activation :: IsMPSGraphLSTMDescriptor mpsGraphLSTMDescriptor => mpsGraphLSTMDescriptor -> IO MPSGraphRNNActivation
activation mpsGraphLSTMDescriptor  =
  fmap (coerce :: CULong -> MPSGraphRNNActivation) $ sendMsg mpsGraphLSTMDescriptor (mkSelector "activation") retCULong []

-- | A parameter that defines the activation function used with the current cell value of the LSTM operation.
--
-- Default value: @MPSGraphRNNActivationTanh@.
--
-- ObjC selector: @- setActivation:@
setActivation :: IsMPSGraphLSTMDescriptor mpsGraphLSTMDescriptor => mpsGraphLSTMDescriptor -> MPSGraphRNNActivation -> IO ()
setActivation mpsGraphLSTMDescriptor  value =
  sendMsg mpsGraphLSTMDescriptor (mkSelector "setActivation:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @reverse@
reverseSelector :: Selector
reverseSelector = mkSelector "reverse"

-- | @Selector@ for @setReverse:@
setReverseSelector :: Selector
setReverseSelector = mkSelector "setReverse:"

-- | @Selector@ for @bidirectional@
bidirectionalSelector :: Selector
bidirectionalSelector = mkSelector "bidirectional"

-- | @Selector@ for @setBidirectional:@
setBidirectionalSelector :: Selector
setBidirectionalSelector = mkSelector "setBidirectional:"

-- | @Selector@ for @produceCell@
produceCellSelector :: Selector
produceCellSelector = mkSelector "produceCell"

-- | @Selector@ for @setProduceCell:@
setProduceCellSelector :: Selector
setProduceCellSelector = mkSelector "setProduceCell:"

-- | @Selector@ for @training@
trainingSelector :: Selector
trainingSelector = mkSelector "training"

-- | @Selector@ for @setTraining:@
setTrainingSelector :: Selector
setTrainingSelector = mkSelector "setTraining:"

-- | @Selector@ for @forgetGateLast@
forgetGateLastSelector :: Selector
forgetGateLastSelector = mkSelector "forgetGateLast"

-- | @Selector@ for @setForgetGateLast:@
setForgetGateLastSelector :: Selector
setForgetGateLastSelector = mkSelector "setForgetGateLast:"

-- | @Selector@ for @inputGateActivation@
inputGateActivationSelector :: Selector
inputGateActivationSelector = mkSelector "inputGateActivation"

-- | @Selector@ for @setInputGateActivation:@
setInputGateActivationSelector :: Selector
setInputGateActivationSelector = mkSelector "setInputGateActivation:"

-- | @Selector@ for @forgetGateActivation@
forgetGateActivationSelector :: Selector
forgetGateActivationSelector = mkSelector "forgetGateActivation"

-- | @Selector@ for @setForgetGateActivation:@
setForgetGateActivationSelector :: Selector
setForgetGateActivationSelector = mkSelector "setForgetGateActivation:"

-- | @Selector@ for @cellGateActivation@
cellGateActivationSelector :: Selector
cellGateActivationSelector = mkSelector "cellGateActivation"

-- | @Selector@ for @setCellGateActivation:@
setCellGateActivationSelector :: Selector
setCellGateActivationSelector = mkSelector "setCellGateActivation:"

-- | @Selector@ for @outputGateActivation@
outputGateActivationSelector :: Selector
outputGateActivationSelector = mkSelector "outputGateActivation"

-- | @Selector@ for @setOutputGateActivation:@
setOutputGateActivationSelector :: Selector
setOutputGateActivationSelector = mkSelector "setOutputGateActivation:"

-- | @Selector@ for @activation@
activationSelector :: Selector
activationSelector = mkSelector "activation"

-- | @Selector@ for @setActivation:@
setActivationSelector :: Selector
setActivationSelector = mkSelector "setActivation:"

