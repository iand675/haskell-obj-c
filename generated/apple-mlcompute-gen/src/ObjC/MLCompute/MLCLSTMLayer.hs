{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCLSTMLayer
--
-- A LSTM layer
--
-- The hidden and cell state for inputs and outputs have a layout of [numberOfLayers, numberOfDirections, batchSize, hiddenSize].
--
-- Generated bindings for @MLCLSTMLayer@.
module ObjC.MLCompute.MLCLSTMLayer
  ( MLCLSTMLayer
  , IsMLCLSTMLayer(..)
  , layerWithDescriptor_inputWeights_hiddenWeights_biases
  , layerWithDescriptor_inputWeights_hiddenWeights_peepholeWeights_biases
  , layerWithDescriptor_inputWeights_hiddenWeights_peepholeWeights_biases_gateActivations_outputResultActivation
  , descriptor
  , gateActivations
  , outputResultActivation
  , inputWeights
  , hiddenWeights
  , peepholeWeights
  , biases
  , inputWeightsParameters
  , hiddenWeightsParameters
  , peepholeWeightsParameters
  , biasesParameters
  , biasesParametersSelector
  , biasesSelector
  , descriptorSelector
  , gateActivationsSelector
  , hiddenWeightsParametersSelector
  , hiddenWeightsSelector
  , inputWeightsParametersSelector
  , inputWeightsSelector
  , layerWithDescriptor_inputWeights_hiddenWeights_biasesSelector
  , layerWithDescriptor_inputWeights_hiddenWeights_peepholeWeights_biasesSelector
  , layerWithDescriptor_inputWeights_hiddenWeights_peepholeWeights_biases_gateActivations_outputResultActivationSelector
  , outputResultActivationSelector
  , peepholeWeightsParametersSelector
  , peepholeWeightsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a LSTM layer
--
-- @descriptor@ — The LSTM descriptor
--
-- @inputWeights@ — An array of (layerCount * 4) tensors describing the input weights for the                                 input, hidden, cell and output gates for layer0, layer1.. layer(n-1) for layerCount=n.
--
-- @hiddenWeights@ — An array of (layerCount * 4) tensors describing the hidden weights for the                                 input, hidden, cell and output gates for layer0, layer1.. layer(n-1) for layerCount=n.
--
-- Returns: A new LSTM layer.
--
-- ObjC selector: @+ layerWithDescriptor:inputWeights:hiddenWeights:biases:@
layerWithDescriptor_inputWeights_hiddenWeights_biases :: (IsMLCLSTMDescriptor descriptor, IsNSArray inputWeights, IsNSArray hiddenWeights, IsNSArray biases) => descriptor -> inputWeights -> hiddenWeights -> biases -> IO (Id MLCLSTMLayer)
layerWithDescriptor_inputWeights_hiddenWeights_biases descriptor inputWeights hiddenWeights biases =
  do
    cls' <- getRequiredClass "MLCLSTMLayer"
    sendClassMessage cls' layerWithDescriptor_inputWeights_hiddenWeights_biasesSelector (toMLCLSTMDescriptor descriptor) (toNSArray inputWeights) (toNSArray hiddenWeights) (toNSArray biases)

-- | Create a LSTM layer
--
-- @descriptor@ — The LSTM descriptor
--
-- @inputWeights@ — An array of (layerCount * 4) tensors describing the input weights for the                                 input, hidden, cell and output gates for layer0, layer1.. layer(n-1) for layerCount=n.
--
-- @hiddenWeights@ — An array of (layerCount * 4) tensors describing the hidden weights for the                                 input, hidden, cell and output gates for layer0, layer1.. layer(n-1) for layerCount=n.
--
-- @peepholeWeights@ — An array of (layerCount * 4) tensors describing the peephole weights for the                                 input, hidden, cell and output gates for layer0, layer1.. layer(n-1) for layerCount=n.
--
-- Returns: A new LSTM layer.
--
-- ObjC selector: @+ layerWithDescriptor:inputWeights:hiddenWeights:peepholeWeights:biases:@
layerWithDescriptor_inputWeights_hiddenWeights_peepholeWeights_biases :: (IsMLCLSTMDescriptor descriptor, IsNSArray inputWeights, IsNSArray hiddenWeights, IsNSArray peepholeWeights, IsNSArray biases) => descriptor -> inputWeights -> hiddenWeights -> peepholeWeights -> biases -> IO (Id MLCLSTMLayer)
layerWithDescriptor_inputWeights_hiddenWeights_peepholeWeights_biases descriptor inputWeights hiddenWeights peepholeWeights biases =
  do
    cls' <- getRequiredClass "MLCLSTMLayer"
    sendClassMessage cls' layerWithDescriptor_inputWeights_hiddenWeights_peepholeWeights_biasesSelector (toMLCLSTMDescriptor descriptor) (toNSArray inputWeights) (toNSArray hiddenWeights) (toNSArray peepholeWeights) (toNSArray biases)

-- | Create a LSTM layer
--
-- @descriptor@ — The LSTM descriptor
--
-- @inputWeights@ — An array of (layerCount * 4) tensors describing the input weights for the                                       input, hidden, cell and output gates for layer0, layer1.. layer(n-1) for layerCount=n.                                       For bidirectional LSTM, the forward time weights for all stacked layers will come first followed by backward time weights
--
-- @hiddenWeights@ — An array of (layerCount * 4) tensors describing the hidden weights for the                                       input, hidden, cell and output gates for layer0, layer1.. layer(n-1) for layerCount=n.                                       For bidirectional LSTM, the forward time weights for all stacked layers will come first followed by backward time weights
--
-- @peepholeWeights@ — An array of (layerCount * 4) tensors describing the peephole weights for the                                       input, hidden, cell and output gates for layer0, layer1.. layer(n-1) for layerCount=n.
--
-- @biases@ — An array of (layerCount * 4) tensors describing the input weights for the                                       input, hidden, cell and output gates for layer0, layer1.. layer(n-1) for layerCount=n.                                       For bidirectional LSTM, the forward time bias terms for all stacked layers will come first followed by backward time bias terms
--
-- @gateActivations@ — An array of 4 neuron descriptors for the input, hidden, cell and output gate activations.
--
-- @outputResultActivation@ — The neuron descriptor used for the activation function applied to output result.  Default is tanh.
--
-- Returns: A new  LSTM layer.
--
-- ObjC selector: @+ layerWithDescriptor:inputWeights:hiddenWeights:peepholeWeights:biases:gateActivations:outputResultActivation:@
layerWithDescriptor_inputWeights_hiddenWeights_peepholeWeights_biases_gateActivations_outputResultActivation :: (IsMLCLSTMDescriptor descriptor, IsNSArray inputWeights, IsNSArray hiddenWeights, IsNSArray peepholeWeights, IsNSArray biases, IsNSArray gateActivations, IsMLCActivationDescriptor outputResultActivation) => descriptor -> inputWeights -> hiddenWeights -> peepholeWeights -> biases -> gateActivations -> outputResultActivation -> IO (Id MLCLSTMLayer)
layerWithDescriptor_inputWeights_hiddenWeights_peepholeWeights_biases_gateActivations_outputResultActivation descriptor inputWeights hiddenWeights peepholeWeights biases gateActivations outputResultActivation =
  do
    cls' <- getRequiredClass "MLCLSTMLayer"
    sendClassMessage cls' layerWithDescriptor_inputWeights_hiddenWeights_peepholeWeights_biases_gateActivations_outputResultActivationSelector (toMLCLSTMDescriptor descriptor) (toNSArray inputWeights) (toNSArray hiddenWeights) (toNSArray peepholeWeights) (toNSArray biases) (toNSArray gateActivations) (toMLCActivationDescriptor outputResultActivation)

-- | descriptor
--
-- The LSTM descriptor
--
-- ObjC selector: @- descriptor@
descriptor :: IsMLCLSTMLayer mlclstmLayer => mlclstmLayer -> IO (Id MLCLSTMDescriptor)
descriptor mlclstmLayer =
  sendMessage mlclstmLayer descriptorSelector

-- | gateActivations
--
-- The array of gate activations for input, hidden, cell and output gates
--
-- The default gate activations are: sigmoid, sigmoid, tanh, sigmoid
--
-- ObjC selector: @- gateActivations@
gateActivations :: IsMLCLSTMLayer mlclstmLayer => mlclstmLayer -> IO (Id NSArray)
gateActivations mlclstmLayer =
  sendMessage mlclstmLayer gateActivationsSelector

-- | outputResultActivation
--
-- The output activation descriptor
--
-- ObjC selector: @- outputResultActivation@
outputResultActivation :: IsMLCLSTMLayer mlclstmLayer => mlclstmLayer -> IO (Id MLCActivationDescriptor)
outputResultActivation mlclstmLayer =
  sendMessage mlclstmLayer outputResultActivationSelector

-- | inputWeights
--
-- The array of tensors describing the input weights for the input, hidden, cell and output gates
--
-- ObjC selector: @- inputWeights@
inputWeights :: IsMLCLSTMLayer mlclstmLayer => mlclstmLayer -> IO (Id NSArray)
inputWeights mlclstmLayer =
  sendMessage mlclstmLayer inputWeightsSelector

-- | hiddenWeights
--
-- The array of tensors describing the hidden weights for the input, hidden, cell and output gates
--
-- ObjC selector: @- hiddenWeights@
hiddenWeights :: IsMLCLSTMLayer mlclstmLayer => mlclstmLayer -> IO (Id NSArray)
hiddenWeights mlclstmLayer =
  sendMessage mlclstmLayer hiddenWeightsSelector

-- | peepholeWeights
--
-- The array of tensors describing the peephole weights for the input, hidden, cell and output gates
--
-- ObjC selector: @- peepholeWeights@
peepholeWeights :: IsMLCLSTMLayer mlclstmLayer => mlclstmLayer -> IO (Id NSArray)
peepholeWeights mlclstmLayer =
  sendMessage mlclstmLayer peepholeWeightsSelector

-- | biases
--
-- The array of tensors describing the bias terms for the input, hidden, cell and output gates
--
-- ObjC selector: @- biases@
biases :: IsMLCLSTMLayer mlclstmLayer => mlclstmLayer -> IO (Id NSArray)
biases mlclstmLayer =
  sendMessage mlclstmLayer biasesSelector

-- | inputWeightsParameters
--
-- The input weights tensor parameters used for optimizer update
--
-- ObjC selector: @- inputWeightsParameters@
inputWeightsParameters :: IsMLCLSTMLayer mlclstmLayer => mlclstmLayer -> IO (Id NSArray)
inputWeightsParameters mlclstmLayer =
  sendMessage mlclstmLayer inputWeightsParametersSelector

-- | hiddenWeightsParameters
--
-- The hidden weights tensor parameters used for optimizer update
--
-- ObjC selector: @- hiddenWeightsParameters@
hiddenWeightsParameters :: IsMLCLSTMLayer mlclstmLayer => mlclstmLayer -> IO (Id NSArray)
hiddenWeightsParameters mlclstmLayer =
  sendMessage mlclstmLayer hiddenWeightsParametersSelector

-- | peepholeWeightsParameters
--
-- The peephole weights tensor parameters used for optimizer update
--
-- ObjC selector: @- peepholeWeightsParameters@
peepholeWeightsParameters :: IsMLCLSTMLayer mlclstmLayer => mlclstmLayer -> IO (Id NSArray)
peepholeWeightsParameters mlclstmLayer =
  sendMessage mlclstmLayer peepholeWeightsParametersSelector

-- | biasesParameters
--
-- The bias tensor parameter used for optimizer update
--
-- ObjC selector: @- biasesParameters@
biasesParameters :: IsMLCLSTMLayer mlclstmLayer => mlclstmLayer -> IO (Id NSArray)
biasesParameters mlclstmLayer =
  sendMessage mlclstmLayer biasesParametersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithDescriptor:inputWeights:hiddenWeights:biases:@
layerWithDescriptor_inputWeights_hiddenWeights_biasesSelector :: Selector '[Id MLCLSTMDescriptor, Id NSArray, Id NSArray, Id NSArray] (Id MLCLSTMLayer)
layerWithDescriptor_inputWeights_hiddenWeights_biasesSelector = mkSelector "layerWithDescriptor:inputWeights:hiddenWeights:biases:"

-- | @Selector@ for @layerWithDescriptor:inputWeights:hiddenWeights:peepholeWeights:biases:@
layerWithDescriptor_inputWeights_hiddenWeights_peepholeWeights_biasesSelector :: Selector '[Id MLCLSTMDescriptor, Id NSArray, Id NSArray, Id NSArray, Id NSArray] (Id MLCLSTMLayer)
layerWithDescriptor_inputWeights_hiddenWeights_peepholeWeights_biasesSelector = mkSelector "layerWithDescriptor:inputWeights:hiddenWeights:peepholeWeights:biases:"

-- | @Selector@ for @layerWithDescriptor:inputWeights:hiddenWeights:peepholeWeights:biases:gateActivations:outputResultActivation:@
layerWithDescriptor_inputWeights_hiddenWeights_peepholeWeights_biases_gateActivations_outputResultActivationSelector :: Selector '[Id MLCLSTMDescriptor, Id NSArray, Id NSArray, Id NSArray, Id NSArray, Id NSArray, Id MLCActivationDescriptor] (Id MLCLSTMLayer)
layerWithDescriptor_inputWeights_hiddenWeights_peepholeWeights_biases_gateActivations_outputResultActivationSelector = mkSelector "layerWithDescriptor:inputWeights:hiddenWeights:peepholeWeights:biases:gateActivations:outputResultActivation:"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id MLCLSTMDescriptor)
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @gateActivations@
gateActivationsSelector :: Selector '[] (Id NSArray)
gateActivationsSelector = mkSelector "gateActivations"

-- | @Selector@ for @outputResultActivation@
outputResultActivationSelector :: Selector '[] (Id MLCActivationDescriptor)
outputResultActivationSelector = mkSelector "outputResultActivation"

-- | @Selector@ for @inputWeights@
inputWeightsSelector :: Selector '[] (Id NSArray)
inputWeightsSelector = mkSelector "inputWeights"

-- | @Selector@ for @hiddenWeights@
hiddenWeightsSelector :: Selector '[] (Id NSArray)
hiddenWeightsSelector = mkSelector "hiddenWeights"

-- | @Selector@ for @peepholeWeights@
peepholeWeightsSelector :: Selector '[] (Id NSArray)
peepholeWeightsSelector = mkSelector "peepholeWeights"

-- | @Selector@ for @biases@
biasesSelector :: Selector '[] (Id NSArray)
biasesSelector = mkSelector "biases"

-- | @Selector@ for @inputWeightsParameters@
inputWeightsParametersSelector :: Selector '[] (Id NSArray)
inputWeightsParametersSelector = mkSelector "inputWeightsParameters"

-- | @Selector@ for @hiddenWeightsParameters@
hiddenWeightsParametersSelector :: Selector '[] (Id NSArray)
hiddenWeightsParametersSelector = mkSelector "hiddenWeightsParameters"

-- | @Selector@ for @peepholeWeightsParameters@
peepholeWeightsParametersSelector :: Selector '[] (Id NSArray)
peepholeWeightsParametersSelector = mkSelector "peepholeWeightsParameters"

-- | @Selector@ for @biasesParameters@
biasesParametersSelector :: Selector '[] (Id NSArray)
biasesParametersSelector = mkSelector "biasesParameters"

