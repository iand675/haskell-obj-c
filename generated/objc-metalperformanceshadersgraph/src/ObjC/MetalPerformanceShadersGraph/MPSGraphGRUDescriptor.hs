{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The class that defines the parameters for a gated recurrent unit (GRU) operation.
--
-- Use this descriptor with the following ``MPSGraph`` methods: - ``MPSGraph/GRUWithSourceTensor:recurrentWeight:inputWeight:bias:descriptor:name:`` - ``MPSGraph/GRUWithSourceTensor:recurrentWeight:inputWeight:bias:initState:descriptor:name:`` - ``MPSGraph/GRUWithSourceTensor:recurrentWeight:inputWeight:bias:initState:mask:secondaryBias:descriptor:name:`` - ``MPSGraph/GRUGradientsWithSourceTensor:recurrentWeight:sourceGradient:zState:outputFwd:inputWeight:bias:descriptor:name:`` - ``MPSGraph/GRUGradientsWithSourceTensor:recurrentWeight:sourceGradient:zState:outputFwd:inputWeight:bias:initState:descriptor:name:`` - ``MPSGraph/GRUGradientsWithSourceTensor:recurrentWeight:sourceGradient:zState:outputFwd:stateGradient:inputWeight:bias:initState:mask:secondaryBias:descriptor:name:``
--
-- Generated bindings for @MPSGraphGRUDescriptor@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphGRUDescriptor
  ( MPSGraphGRUDescriptor
  , IsMPSGraphGRUDescriptor(..)
  , descriptor
  , reverse_
  , setReverse
  , bidirectional
  , setBidirectional
  , training
  , setTraining
  , resetGateFirst
  , setResetGateFirst
  , resetAfter
  , setResetAfter
  , flipZ
  , setFlipZ
  , updateGateActivation
  , setUpdateGateActivation
  , resetGateActivation
  , setResetGateActivation
  , outputGateActivation
  , setOutputGateActivation
  , descriptorSelector
  , reverseSelector
  , setReverseSelector
  , bidirectionalSelector
  , setBidirectionalSelector
  , trainingSelector
  , setTrainingSelector
  , resetGateFirstSelector
  , setResetGateFirstSelector
  , resetAfterSelector
  , setResetAfterSelector
  , flipZSelector
  , setFlipZSelector
  , updateGateActivationSelector
  , setUpdateGateActivationSelector
  , resetGateActivationSelector
  , setResetGateActivationSelector
  , outputGateActivationSelector
  , setOutputGateActivationSelector

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

-- | Creates an GRU descriptor with default values.
--
-- ObjC selector: @+ descriptor@
descriptor :: IO (Id MPSGraphGRUDescriptor)
descriptor  =
  do
    cls' <- getRequiredClass "MPSGraphGRUDescriptor"
    sendClassMsg cls' (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A parameter that defines the time direction of the input sequence.
--
-- If set to @YES@ then the input sequence is passed in reverse time order to the layer. Note: Ignored when @bidirectional = YES@. Default value: @NO@.
--
-- ObjC selector: @- reverse@
reverse_ :: IsMPSGraphGRUDescriptor mpsGraphGRUDescriptor => mpsGraphGRUDescriptor -> IO Bool
reverse_ mpsGraphGRUDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsGraphGRUDescriptor (mkSelector "reverse") retCULong []

-- | A parameter that defines the time direction of the input sequence.
--
-- If set to @YES@ then the input sequence is passed in reverse time order to the layer. Note: Ignored when @bidirectional = YES@. Default value: @NO@.
--
-- ObjC selector: @- setReverse:@
setReverse :: IsMPSGraphGRUDescriptor mpsGraphGRUDescriptor => mpsGraphGRUDescriptor -> Bool -> IO ()
setReverse mpsGraphGRUDescriptor  value =
  sendMsg mpsGraphGRUDescriptor (mkSelector "setReverse:") retVoid [argCULong (if value then 1 else 0)]

-- | A parameter that defines a bidirectional GRU layer.
--
-- If set to @YES@ then the input sequence is traversed in both directions and the two results are concatenated together on the channel-axis. Default value: @NO@.
--
-- ObjC selector: @- bidirectional@
bidirectional :: IsMPSGraphGRUDescriptor mpsGraphGRUDescriptor => mpsGraphGRUDescriptor -> IO Bool
bidirectional mpsGraphGRUDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsGraphGRUDescriptor (mkSelector "bidirectional") retCULong []

-- | A parameter that defines a bidirectional GRU layer.
--
-- If set to @YES@ then the input sequence is traversed in both directions and the two results are concatenated together on the channel-axis. Default value: @NO@.
--
-- ObjC selector: @- setBidirectional:@
setBidirectional :: IsMPSGraphGRUDescriptor mpsGraphGRUDescriptor => mpsGraphGRUDescriptor -> Bool -> IO ()
setBidirectional mpsGraphGRUDescriptor  value =
  sendMsg mpsGraphGRUDescriptor (mkSelector "setBidirectional:") retVoid [argCULong (if value then 1 else 0)]

-- | A parameter that enables the GRU layer to support training.
--
-- If set to @YES@ then the layer will produce training state tensor as a secondary output. Default value: @NO@.
--
-- ObjC selector: @- training@
training :: IsMPSGraphGRUDescriptor mpsGraphGRUDescriptor => mpsGraphGRUDescriptor -> IO Bool
training mpsGraphGRUDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsGraphGRUDescriptor (mkSelector "training") retCULong []

-- | A parameter that enables the GRU layer to support training.
--
-- If set to @YES@ then the layer will produce training state tensor as a secondary output. Default value: @NO@.
--
-- ObjC selector: @- setTraining:@
setTraining :: IsMPSGraphGRUDescriptor mpsGraphGRUDescriptor => mpsGraphGRUDescriptor -> Bool -> IO ()
setTraining mpsGraphGRUDescriptor  value =
  sendMsg mpsGraphGRUDescriptor (mkSelector "setTraining:") retVoid [argCULong (if value then 1 else 0)]

-- | A parameter that controls the internal order of the GRU gates.
--
-- If set to @YES@ then the layer will use the gate-ordering @[ r, z, o ]@ instead of default @[ z, r, o ]@. Default value: @NO@.
--
-- ObjC selector: @- resetGateFirst@
resetGateFirst :: IsMPSGraphGRUDescriptor mpsGraphGRUDescriptor => mpsGraphGRUDescriptor -> IO Bool
resetGateFirst mpsGraphGRUDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsGraphGRUDescriptor (mkSelector "resetGateFirst") retCULong []

-- | A parameter that controls the internal order of the GRU gates.
--
-- If set to @YES@ then the layer will use the gate-ordering @[ r, z, o ]@ instead of default @[ z, r, o ]@. Default value: @NO@.
--
-- ObjC selector: @- setResetGateFirst:@
setResetGateFirst :: IsMPSGraphGRUDescriptor mpsGraphGRUDescriptor => mpsGraphGRUDescriptor -> Bool -> IO ()
setResetGateFirst mpsGraphGRUDescriptor  value =
  sendMsg mpsGraphGRUDescriptor (mkSelector "setResetGateFirst:") retVoid [argCULong (if value then 1 else 0)]

-- | A parameter that chooses between two variants for the reset gate computation.
--
-- If set to @YES@ then the layer will compute the intermediate value as @c[t] = ( b + (h[t-1] m ) R^T) r[t]@. Otherwise it's computed as @c[t] = (h[t-1] r[t] m) R^T@. Default value: @NO@.
--
-- ObjC selector: @- resetAfter@
resetAfter :: IsMPSGraphGRUDescriptor mpsGraphGRUDescriptor => mpsGraphGRUDescriptor -> IO Bool
resetAfter mpsGraphGRUDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsGraphGRUDescriptor (mkSelector "resetAfter") retCULong []

-- | A parameter that chooses between two variants for the reset gate computation.
--
-- If set to @YES@ then the layer will compute the intermediate value as @c[t] = ( b + (h[t-1] m ) R^T) r[t]@. Otherwise it's computed as @c[t] = (h[t-1] r[t] m) R^T@. Default value: @NO@.
--
-- ObjC selector: @- setResetAfter:@
setResetAfter :: IsMPSGraphGRUDescriptor mpsGraphGRUDescriptor => mpsGraphGRUDescriptor -> Bool -> IO ()
setResetAfter mpsGraphGRUDescriptor  value =
  sendMsg mpsGraphGRUDescriptor (mkSelector "setResetAfter:") retVoid [argCULong (if value then 1 else 0)]

-- | A parameter that chooses between two variants for the final output computation.
--
-- If set to @YES@ then the layer will compute the final value as @h[t] = z[t] h[t-1] + (1-z[t]) o[t]@. Otherwise it's computed as @h[t] = (1-z[t]) h[t-1] + z[t] o[t]@. Default value: @NO@.
--
-- ObjC selector: @- flipZ@
flipZ :: IsMPSGraphGRUDescriptor mpsGraphGRUDescriptor => mpsGraphGRUDescriptor -> IO Bool
flipZ mpsGraphGRUDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsGraphGRUDescriptor (mkSelector "flipZ") retCULong []

-- | A parameter that chooses between two variants for the final output computation.
--
-- If set to @YES@ then the layer will compute the final value as @h[t] = z[t] h[t-1] + (1-z[t]) o[t]@. Otherwise it's computed as @h[t] = (1-z[t]) h[t-1] + z[t] o[t]@. Default value: @NO@.
--
-- ObjC selector: @- setFlipZ:@
setFlipZ :: IsMPSGraphGRUDescriptor mpsGraphGRUDescriptor => mpsGraphGRUDescriptor -> Bool -> IO ()
setFlipZ mpsGraphGRUDescriptor  value =
  sendMsg mpsGraphGRUDescriptor (mkSelector "setFlipZ:") retVoid [argCULong (if value then 1 else 0)]

-- | A parameter that defines the activation function to use with the update-gate of the GRU operation.
--
-- Default value: @MPSGraphRNNActivationSigmoid@.
--
-- ObjC selector: @- updateGateActivation@
updateGateActivation :: IsMPSGraphGRUDescriptor mpsGraphGRUDescriptor => mpsGraphGRUDescriptor -> IO MPSGraphRNNActivation
updateGateActivation mpsGraphGRUDescriptor  =
  fmap (coerce :: CULong -> MPSGraphRNNActivation) $ sendMsg mpsGraphGRUDescriptor (mkSelector "updateGateActivation") retCULong []

-- | A parameter that defines the activation function to use with the update-gate of the GRU operation.
--
-- Default value: @MPSGraphRNNActivationSigmoid@.
--
-- ObjC selector: @- setUpdateGateActivation:@
setUpdateGateActivation :: IsMPSGraphGRUDescriptor mpsGraphGRUDescriptor => mpsGraphGRUDescriptor -> MPSGraphRNNActivation -> IO ()
setUpdateGateActivation mpsGraphGRUDescriptor  value =
  sendMsg mpsGraphGRUDescriptor (mkSelector "setUpdateGateActivation:") retVoid [argCULong (coerce value)]

-- | A parameter that defines the activation function to use with the reset-gate of the GRU operation.
--
-- Default value: @MPSGraphRNNActivationSigmoid@.
--
-- ObjC selector: @- resetGateActivation@
resetGateActivation :: IsMPSGraphGRUDescriptor mpsGraphGRUDescriptor => mpsGraphGRUDescriptor -> IO MPSGraphRNNActivation
resetGateActivation mpsGraphGRUDescriptor  =
  fmap (coerce :: CULong -> MPSGraphRNNActivation) $ sendMsg mpsGraphGRUDescriptor (mkSelector "resetGateActivation") retCULong []

-- | A parameter that defines the activation function to use with the reset-gate of the GRU operation.
--
-- Default value: @MPSGraphRNNActivationSigmoid@.
--
-- ObjC selector: @- setResetGateActivation:@
setResetGateActivation :: IsMPSGraphGRUDescriptor mpsGraphGRUDescriptor => mpsGraphGRUDescriptor -> MPSGraphRNNActivation -> IO ()
setResetGateActivation mpsGraphGRUDescriptor  value =
  sendMsg mpsGraphGRUDescriptor (mkSelector "setResetGateActivation:") retVoid [argCULong (coerce value)]

-- | A parameter that defines the activation function to use with the output-gate of the GRU operation.
--
-- Default value: @MPSGraphRNNActivationTanh@.
--
-- ObjC selector: @- outputGateActivation@
outputGateActivation :: IsMPSGraphGRUDescriptor mpsGraphGRUDescriptor => mpsGraphGRUDescriptor -> IO MPSGraphRNNActivation
outputGateActivation mpsGraphGRUDescriptor  =
  fmap (coerce :: CULong -> MPSGraphRNNActivation) $ sendMsg mpsGraphGRUDescriptor (mkSelector "outputGateActivation") retCULong []

-- | A parameter that defines the activation function to use with the output-gate of the GRU operation.
--
-- Default value: @MPSGraphRNNActivationTanh@.
--
-- ObjC selector: @- setOutputGateActivation:@
setOutputGateActivation :: IsMPSGraphGRUDescriptor mpsGraphGRUDescriptor => mpsGraphGRUDescriptor -> MPSGraphRNNActivation -> IO ()
setOutputGateActivation mpsGraphGRUDescriptor  value =
  sendMsg mpsGraphGRUDescriptor (mkSelector "setOutputGateActivation:") retVoid [argCULong (coerce value)]

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

-- | @Selector@ for @resetGateFirst@
resetGateFirstSelector :: Selector
resetGateFirstSelector = mkSelector "resetGateFirst"

-- | @Selector@ for @setResetGateFirst:@
setResetGateFirstSelector :: Selector
setResetGateFirstSelector = mkSelector "setResetGateFirst:"

-- | @Selector@ for @resetAfter@
resetAfterSelector :: Selector
resetAfterSelector = mkSelector "resetAfter"

-- | @Selector@ for @setResetAfter:@
setResetAfterSelector :: Selector
setResetAfterSelector = mkSelector "setResetAfter:"

-- | @Selector@ for @flipZ@
flipZSelector :: Selector
flipZSelector = mkSelector "flipZ"

-- | @Selector@ for @setFlipZ:@
setFlipZSelector :: Selector
setFlipZSelector = mkSelector "setFlipZ:"

-- | @Selector@ for @updateGateActivation@
updateGateActivationSelector :: Selector
updateGateActivationSelector = mkSelector "updateGateActivation"

-- | @Selector@ for @setUpdateGateActivation:@
setUpdateGateActivationSelector :: Selector
setUpdateGateActivationSelector = mkSelector "setUpdateGateActivation:"

-- | @Selector@ for @resetGateActivation@
resetGateActivationSelector :: Selector
resetGateActivationSelector = mkSelector "resetGateActivation"

-- | @Selector@ for @setResetGateActivation:@
setResetGateActivationSelector :: Selector
setResetGateActivationSelector = mkSelector "setResetGateActivation:"

-- | @Selector@ for @outputGateActivation@
outputGateActivationSelector :: Selector
outputGateActivationSelector = mkSelector "outputGateActivation"

-- | @Selector@ for @setOutputGateActivation:@
setOutputGateActivationSelector :: Selector
setOutputGateActivationSelector = mkSelector "setOutputGateActivation:"

