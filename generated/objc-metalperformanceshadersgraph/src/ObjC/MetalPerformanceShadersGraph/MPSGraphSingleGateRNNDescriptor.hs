{-# LANGUAGE PatternSynonyms #-}
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
  , descriptorSelector
  , reverseSelector
  , setReverseSelector
  , bidirectionalSelector
  , setBidirectionalSelector
  , trainingSelector
  , setTrainingSelector
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

-- | Creates a single gate RNN descriptor with default values.
--
-- ObjC selector: @+ descriptor@
descriptor :: IO (Id MPSGraphSingleGateRNNDescriptor)
descriptor  =
  do
    cls' <- getRequiredClass "MPSGraphSingleGateRNNDescriptor"
    sendClassMsg cls' (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A parameter that defines time direction of the input sequence.
--
-- If set to @YES@ then the input sequence is passed in reverse time order to the layer. Note: Ignored when @bidirectional = YES@. Default value: @NO@.
--
-- ObjC selector: @- reverse@
reverse_ :: IsMPSGraphSingleGateRNNDescriptor mpsGraphSingleGateRNNDescriptor => mpsGraphSingleGateRNNDescriptor -> IO Bool
reverse_ mpsGraphSingleGateRNNDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsGraphSingleGateRNNDescriptor (mkSelector "reverse") retCULong []

-- | A parameter that defines time direction of the input sequence.
--
-- If set to @YES@ then the input sequence is passed in reverse time order to the layer. Note: Ignored when @bidirectional = YES@. Default value: @NO@.
--
-- ObjC selector: @- setReverse:@
setReverse :: IsMPSGraphSingleGateRNNDescriptor mpsGraphSingleGateRNNDescriptor => mpsGraphSingleGateRNNDescriptor -> Bool -> IO ()
setReverse mpsGraphSingleGateRNNDescriptor  value =
  sendMsg mpsGraphSingleGateRNNDescriptor (mkSelector "setReverse:") retVoid [argCULong (if value then 1 else 0)]

-- | A parameter that defines a bidirectional RNN layer.
--
-- If set to @YES@ then the input sequence is traversed in both directions and the two results are concatenated together on the channel-axis. Default value: @NO@.
--
-- ObjC selector: @- bidirectional@
bidirectional :: IsMPSGraphSingleGateRNNDescriptor mpsGraphSingleGateRNNDescriptor => mpsGraphSingleGateRNNDescriptor -> IO Bool
bidirectional mpsGraphSingleGateRNNDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsGraphSingleGateRNNDescriptor (mkSelector "bidirectional") retCULong []

-- | A parameter that defines a bidirectional RNN layer.
--
-- If set to @YES@ then the input sequence is traversed in both directions and the two results are concatenated together on the channel-axis. Default value: @NO@.
--
-- ObjC selector: @- setBidirectional:@
setBidirectional :: IsMPSGraphSingleGateRNNDescriptor mpsGraphSingleGateRNNDescriptor => mpsGraphSingleGateRNNDescriptor -> Bool -> IO ()
setBidirectional mpsGraphSingleGateRNNDescriptor  value =
  sendMsg mpsGraphSingleGateRNNDescriptor (mkSelector "setBidirectional:") retVoid [argCULong (if value then 1 else 0)]

-- | A parameter that makes the RNN layer support training.
--
-- If set to @YES@ then the layer will produce training state tensor as a secondary output. Default value: @NO@.
--
-- ObjC selector: @- training@
training :: IsMPSGraphSingleGateRNNDescriptor mpsGraphSingleGateRNNDescriptor => mpsGraphSingleGateRNNDescriptor -> IO Bool
training mpsGraphSingleGateRNNDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsGraphSingleGateRNNDescriptor (mkSelector "training") retCULong []

-- | A parameter that makes the RNN layer support training.
--
-- If set to @YES@ then the layer will produce training state tensor as a secondary output. Default value: @NO@.
--
-- ObjC selector: @- setTraining:@
setTraining :: IsMPSGraphSingleGateRNNDescriptor mpsGraphSingleGateRNNDescriptor => mpsGraphSingleGateRNNDescriptor -> Bool -> IO ()
setTraining mpsGraphSingleGateRNNDescriptor  value =
  sendMsg mpsGraphSingleGateRNNDescriptor (mkSelector "setTraining:") retVoid [argCULong (if value then 1 else 0)]

-- | A parameter that defines the activation function to use with the RNN operation.
--
-- Default value: @MPSGraphRNNActivationRelu@.
--
-- ObjC selector: @- activation@
activation :: IsMPSGraphSingleGateRNNDescriptor mpsGraphSingleGateRNNDescriptor => mpsGraphSingleGateRNNDescriptor -> IO MPSGraphRNNActivation
activation mpsGraphSingleGateRNNDescriptor  =
  fmap (coerce :: CULong -> MPSGraphRNNActivation) $ sendMsg mpsGraphSingleGateRNNDescriptor (mkSelector "activation") retCULong []

-- | A parameter that defines the activation function to use with the RNN operation.
--
-- Default value: @MPSGraphRNNActivationRelu@.
--
-- ObjC selector: @- setActivation:@
setActivation :: IsMPSGraphSingleGateRNNDescriptor mpsGraphSingleGateRNNDescriptor => mpsGraphSingleGateRNNDescriptor -> MPSGraphRNNActivation -> IO ()
setActivation mpsGraphSingleGateRNNDescriptor  value =
  sendMsg mpsGraphSingleGateRNNDescriptor (mkSelector "setActivation:") retVoid [argCULong (coerce value)]

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

-- | @Selector@ for @training@
trainingSelector :: Selector
trainingSelector = mkSelector "training"

-- | @Selector@ for @setTraining:@
setTrainingSelector :: Selector
setTrainingSelector = mkSelector "setTraining:"

-- | @Selector@ for @activation@
activationSelector :: Selector
activationSelector = mkSelector "activation"

-- | @Selector@ for @setActivation:@
setActivationSelector :: Selector
setActivationSelector = mkSelector "setActivation:"

