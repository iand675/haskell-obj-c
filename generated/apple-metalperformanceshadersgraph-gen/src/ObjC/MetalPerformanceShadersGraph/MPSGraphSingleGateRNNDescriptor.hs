{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The class that defines the parameters for a single gate RNN operation.
--
-- Use this descriptor with the following ``MPSGraph`` methods: - ``MPSGraph/singleGateRNNWithSourceTensor:recurrentWeight:initState:descriptor:name:`` - ``MPSGraph/singleGateRNNWithSourceTensor:recurrentWeight:inputWeight:bias:initState:descriptor:name:`` - ``MPSGraph/singleGateRNNWithSourceTensor:recurrentWeight:inputWeight:bias:initState:mask:descriptor:name:`` - ``MPSGraph/singleGateRNNGradientsWithSourceTensor:recurrentWeight:sourceGradient:zState:initState:descriptor:name:`` - ``MPSGraph/singleGateRNNGradientsWithSourceTensor:recurrentWeight:sourceGradient:zState:inputWeight:bias:initState:descriptor:name:`` - ``MPSGraph/singleGateRNNGradientsWithSourceTensor:recurrentWeight:sourceGradient:zState:inputWeight:bias:initState:mask:descriptor:name:`` - ``MPSGraph/singleGateRNNGradientsWithSourceTensor:recurrentWeight:sourceGradient:zState:stateGradient:inputWeight:bias:initState:mask:descriptor:name:``
--
-- Generated bindings for @MPSGraphSingleGateRNNDescriptor@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphSingleGateRNNDescriptor
  ( MPSGraphSingleGateRNNDescriptor
  , IsMPSGraphSingleGateRNNDescriptor(..)
  , descriptor
  , reverse_
  , setReverse
  , bidirectional
  , setBidirectional
  , training
  , setTraining
  , activation
  , setActivation
  , activationSelector
  , bidirectionalSelector
  , descriptorSelector
  , reverseSelector
  , setActivationSelector
  , setBidirectionalSelector
  , setReverseSelector
  , setTrainingSelector
  , trainingSelector

  -- * Enum types
  , MPSGraphRNNActivation(MPSGraphRNNActivation)
  , pattern MPSGraphRNNActivationNone
  , pattern MPSGraphRNNActivationRelu
  , pattern MPSGraphRNNActivationTanh
  , pattern MPSGraphRNNActivationSigmoid
  , pattern MPSGraphRNNActivationHardSigmoid

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShadersGraph.Internal.Classes
import ObjC.MetalPerformanceShadersGraph.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a single gate RNN descriptor with default values.
--
-- ObjC selector: @+ descriptor@
descriptor :: IO (Id MPSGraphSingleGateRNNDescriptor)
descriptor  =
  do
    cls' <- getRequiredClass "MPSGraphSingleGateRNNDescriptor"
    sendClassMessage cls' descriptorSelector

-- | A parameter that defines time direction of the input sequence.
--
-- If set to @YES@ then the input sequence is passed in reverse time order to the layer. Note: Ignored when @bidirectional = YES@. Default value: @NO@.
--
-- ObjC selector: @- reverse@
reverse_ :: IsMPSGraphSingleGateRNNDescriptor mpsGraphSingleGateRNNDescriptor => mpsGraphSingleGateRNNDescriptor -> IO Bool
reverse_ mpsGraphSingleGateRNNDescriptor =
  sendMessage mpsGraphSingleGateRNNDescriptor reverseSelector

-- | A parameter that defines time direction of the input sequence.
--
-- If set to @YES@ then the input sequence is passed in reverse time order to the layer. Note: Ignored when @bidirectional = YES@. Default value: @NO@.
--
-- ObjC selector: @- setReverse:@
setReverse :: IsMPSGraphSingleGateRNNDescriptor mpsGraphSingleGateRNNDescriptor => mpsGraphSingleGateRNNDescriptor -> Bool -> IO ()
setReverse mpsGraphSingleGateRNNDescriptor value =
  sendMessage mpsGraphSingleGateRNNDescriptor setReverseSelector value

-- | A parameter that defines a bidirectional RNN layer.
--
-- If set to @YES@ then the input sequence is traversed in both directions and the two results are concatenated together on the channel-axis. Default value: @NO@.
--
-- ObjC selector: @- bidirectional@
bidirectional :: IsMPSGraphSingleGateRNNDescriptor mpsGraphSingleGateRNNDescriptor => mpsGraphSingleGateRNNDescriptor -> IO Bool
bidirectional mpsGraphSingleGateRNNDescriptor =
  sendMessage mpsGraphSingleGateRNNDescriptor bidirectionalSelector

-- | A parameter that defines a bidirectional RNN layer.
--
-- If set to @YES@ then the input sequence is traversed in both directions and the two results are concatenated together on the channel-axis. Default value: @NO@.
--
-- ObjC selector: @- setBidirectional:@
setBidirectional :: IsMPSGraphSingleGateRNNDescriptor mpsGraphSingleGateRNNDescriptor => mpsGraphSingleGateRNNDescriptor -> Bool -> IO ()
setBidirectional mpsGraphSingleGateRNNDescriptor value =
  sendMessage mpsGraphSingleGateRNNDescriptor setBidirectionalSelector value

-- | A parameter that makes the RNN layer support training.
--
-- If set to @YES@ then the layer will produce training state tensor as a secondary output. Default value: @NO@.
--
-- ObjC selector: @- training@
training :: IsMPSGraphSingleGateRNNDescriptor mpsGraphSingleGateRNNDescriptor => mpsGraphSingleGateRNNDescriptor -> IO Bool
training mpsGraphSingleGateRNNDescriptor =
  sendMessage mpsGraphSingleGateRNNDescriptor trainingSelector

-- | A parameter that makes the RNN layer support training.
--
-- If set to @YES@ then the layer will produce training state tensor as a secondary output. Default value: @NO@.
--
-- ObjC selector: @- setTraining:@
setTraining :: IsMPSGraphSingleGateRNNDescriptor mpsGraphSingleGateRNNDescriptor => mpsGraphSingleGateRNNDescriptor -> Bool -> IO ()
setTraining mpsGraphSingleGateRNNDescriptor value =
  sendMessage mpsGraphSingleGateRNNDescriptor setTrainingSelector value

-- | A parameter that defines the activation function to use with the RNN operation.
--
-- Default value: @MPSGraphRNNActivationRelu@.
--
-- ObjC selector: @- activation@
activation :: IsMPSGraphSingleGateRNNDescriptor mpsGraphSingleGateRNNDescriptor => mpsGraphSingleGateRNNDescriptor -> IO MPSGraphRNNActivation
activation mpsGraphSingleGateRNNDescriptor =
  sendMessage mpsGraphSingleGateRNNDescriptor activationSelector

-- | A parameter that defines the activation function to use with the RNN operation.
--
-- Default value: @MPSGraphRNNActivationRelu@.
--
-- ObjC selector: @- setActivation:@
setActivation :: IsMPSGraphSingleGateRNNDescriptor mpsGraphSingleGateRNNDescriptor => mpsGraphSingleGateRNNDescriptor -> MPSGraphRNNActivation -> IO ()
setActivation mpsGraphSingleGateRNNDescriptor value =
  sendMessage mpsGraphSingleGateRNNDescriptor setActivationSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id MPSGraphSingleGateRNNDescriptor)
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @reverse@
reverseSelector :: Selector '[] Bool
reverseSelector = mkSelector "reverse"

-- | @Selector@ for @setReverse:@
setReverseSelector :: Selector '[Bool] ()
setReverseSelector = mkSelector "setReverse:"

-- | @Selector@ for @bidirectional@
bidirectionalSelector :: Selector '[] Bool
bidirectionalSelector = mkSelector "bidirectional"

-- | @Selector@ for @setBidirectional:@
setBidirectionalSelector :: Selector '[Bool] ()
setBidirectionalSelector = mkSelector "setBidirectional:"

-- | @Selector@ for @training@
trainingSelector :: Selector '[] Bool
trainingSelector = mkSelector "training"

-- | @Selector@ for @setTraining:@
setTrainingSelector :: Selector '[Bool] ()
setTrainingSelector = mkSelector "setTraining:"

-- | @Selector@ for @activation@
activationSelector :: Selector '[] MPSGraphRNNActivation
activationSelector = mkSelector "activation"

-- | @Selector@ for @setActivation:@
setActivationSelector :: Selector '[MPSGraphRNNActivation] ()
setActivationSelector = mkSelector "setActivation:"

