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
  , layerWithDescriptor_inputWeights_hiddenWeights_biasesSelector
  , layerWithDescriptor_inputWeights_hiddenWeights_peepholeWeights_biasesSelector
  , layerWithDescriptor_inputWeights_hiddenWeights_peepholeWeights_biases_gateActivations_outputResultActivationSelector
  , descriptorSelector
  , gateActivationsSelector
  , outputResultActivationSelector
  , inputWeightsSelector
  , hiddenWeightsSelector
  , peepholeWeightsSelector
  , biasesSelector
  , inputWeightsParametersSelector
  , hiddenWeightsParametersSelector
  , peepholeWeightsParametersSelector
  , biasesParametersSelector


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
    withObjCPtr descriptor $ \raw_descriptor ->
      withObjCPtr inputWeights $ \raw_inputWeights ->
        withObjCPtr hiddenWeights $ \raw_hiddenWeights ->
          withObjCPtr biases $ \raw_biases ->
            sendClassMsg cls' (mkSelector "layerWithDescriptor:inputWeights:hiddenWeights:biases:") (retPtr retVoid) [argPtr (castPtr raw_descriptor :: Ptr ()), argPtr (castPtr raw_inputWeights :: Ptr ()), argPtr (castPtr raw_hiddenWeights :: Ptr ()), argPtr (castPtr raw_biases :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr descriptor $ \raw_descriptor ->
      withObjCPtr inputWeights $ \raw_inputWeights ->
        withObjCPtr hiddenWeights $ \raw_hiddenWeights ->
          withObjCPtr peepholeWeights $ \raw_peepholeWeights ->
            withObjCPtr biases $ \raw_biases ->
              sendClassMsg cls' (mkSelector "layerWithDescriptor:inputWeights:hiddenWeights:peepholeWeights:biases:") (retPtr retVoid) [argPtr (castPtr raw_descriptor :: Ptr ()), argPtr (castPtr raw_inputWeights :: Ptr ()), argPtr (castPtr raw_hiddenWeights :: Ptr ()), argPtr (castPtr raw_peepholeWeights :: Ptr ()), argPtr (castPtr raw_biases :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr descriptor $ \raw_descriptor ->
      withObjCPtr inputWeights $ \raw_inputWeights ->
        withObjCPtr hiddenWeights $ \raw_hiddenWeights ->
          withObjCPtr peepholeWeights $ \raw_peepholeWeights ->
            withObjCPtr biases $ \raw_biases ->
              withObjCPtr gateActivations $ \raw_gateActivations ->
                withObjCPtr outputResultActivation $ \raw_outputResultActivation ->
                  sendClassMsg cls' (mkSelector "layerWithDescriptor:inputWeights:hiddenWeights:peepholeWeights:biases:gateActivations:outputResultActivation:") (retPtr retVoid) [argPtr (castPtr raw_descriptor :: Ptr ()), argPtr (castPtr raw_inputWeights :: Ptr ()), argPtr (castPtr raw_hiddenWeights :: Ptr ()), argPtr (castPtr raw_peepholeWeights :: Ptr ()), argPtr (castPtr raw_biases :: Ptr ()), argPtr (castPtr raw_gateActivations :: Ptr ()), argPtr (castPtr raw_outputResultActivation :: Ptr ())] >>= retainedObject . castPtr

-- | descriptor
--
-- The LSTM descriptor
--
-- ObjC selector: @- descriptor@
descriptor :: IsMLCLSTMLayer mlclstmLayer => mlclstmLayer -> IO (Id MLCLSTMDescriptor)
descriptor mlclstmLayer  =
  sendMsg mlclstmLayer (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | gateActivations
--
-- The array of gate activations for input, hidden, cell and output gates
--
-- The default gate activations are: sigmoid, sigmoid, tanh, sigmoid
--
-- ObjC selector: @- gateActivations@
gateActivations :: IsMLCLSTMLayer mlclstmLayer => mlclstmLayer -> IO (Id NSArray)
gateActivations mlclstmLayer  =
  sendMsg mlclstmLayer (mkSelector "gateActivations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | outputResultActivation
--
-- The output activation descriptor
--
-- ObjC selector: @- outputResultActivation@
outputResultActivation :: IsMLCLSTMLayer mlclstmLayer => mlclstmLayer -> IO (Id MLCActivationDescriptor)
outputResultActivation mlclstmLayer  =
  sendMsg mlclstmLayer (mkSelector "outputResultActivation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | inputWeights
--
-- The array of tensors describing the input weights for the input, hidden, cell and output gates
--
-- ObjC selector: @- inputWeights@
inputWeights :: IsMLCLSTMLayer mlclstmLayer => mlclstmLayer -> IO (Id NSArray)
inputWeights mlclstmLayer  =
  sendMsg mlclstmLayer (mkSelector "inputWeights") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | hiddenWeights
--
-- The array of tensors describing the hidden weights for the input, hidden, cell and output gates
--
-- ObjC selector: @- hiddenWeights@
hiddenWeights :: IsMLCLSTMLayer mlclstmLayer => mlclstmLayer -> IO (Id NSArray)
hiddenWeights mlclstmLayer  =
  sendMsg mlclstmLayer (mkSelector "hiddenWeights") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | peepholeWeights
--
-- The array of tensors describing the peephole weights for the input, hidden, cell and output gates
--
-- ObjC selector: @- peepholeWeights@
peepholeWeights :: IsMLCLSTMLayer mlclstmLayer => mlclstmLayer -> IO (Id NSArray)
peepholeWeights mlclstmLayer  =
  sendMsg mlclstmLayer (mkSelector "peepholeWeights") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | biases
--
-- The array of tensors describing the bias terms for the input, hidden, cell and output gates
--
-- ObjC selector: @- biases@
biases :: IsMLCLSTMLayer mlclstmLayer => mlclstmLayer -> IO (Id NSArray)
biases mlclstmLayer  =
  sendMsg mlclstmLayer (mkSelector "biases") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | inputWeightsParameters
--
-- The input weights tensor parameters used for optimizer update
--
-- ObjC selector: @- inputWeightsParameters@
inputWeightsParameters :: IsMLCLSTMLayer mlclstmLayer => mlclstmLayer -> IO (Id NSArray)
inputWeightsParameters mlclstmLayer  =
  sendMsg mlclstmLayer (mkSelector "inputWeightsParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | hiddenWeightsParameters
--
-- The hidden weights tensor parameters used for optimizer update
--
-- ObjC selector: @- hiddenWeightsParameters@
hiddenWeightsParameters :: IsMLCLSTMLayer mlclstmLayer => mlclstmLayer -> IO (Id NSArray)
hiddenWeightsParameters mlclstmLayer  =
  sendMsg mlclstmLayer (mkSelector "hiddenWeightsParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | peepholeWeightsParameters
--
-- The peephole weights tensor parameters used for optimizer update
--
-- ObjC selector: @- peepholeWeightsParameters@
peepholeWeightsParameters :: IsMLCLSTMLayer mlclstmLayer => mlclstmLayer -> IO (Id NSArray)
peepholeWeightsParameters mlclstmLayer  =
  sendMsg mlclstmLayer (mkSelector "peepholeWeightsParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | biasesParameters
--
-- The bias tensor parameter used for optimizer update
--
-- ObjC selector: @- biasesParameters@
biasesParameters :: IsMLCLSTMLayer mlclstmLayer => mlclstmLayer -> IO (Id NSArray)
biasesParameters mlclstmLayer  =
  sendMsg mlclstmLayer (mkSelector "biasesParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithDescriptor:inputWeights:hiddenWeights:biases:@
layerWithDescriptor_inputWeights_hiddenWeights_biasesSelector :: Selector
layerWithDescriptor_inputWeights_hiddenWeights_biasesSelector = mkSelector "layerWithDescriptor:inputWeights:hiddenWeights:biases:"

-- | @Selector@ for @layerWithDescriptor:inputWeights:hiddenWeights:peepholeWeights:biases:@
layerWithDescriptor_inputWeights_hiddenWeights_peepholeWeights_biasesSelector :: Selector
layerWithDescriptor_inputWeights_hiddenWeights_peepholeWeights_biasesSelector = mkSelector "layerWithDescriptor:inputWeights:hiddenWeights:peepholeWeights:biases:"

-- | @Selector@ for @layerWithDescriptor:inputWeights:hiddenWeights:peepholeWeights:biases:gateActivations:outputResultActivation:@
layerWithDescriptor_inputWeights_hiddenWeights_peepholeWeights_biases_gateActivations_outputResultActivationSelector :: Selector
layerWithDescriptor_inputWeights_hiddenWeights_peepholeWeights_biases_gateActivations_outputResultActivationSelector = mkSelector "layerWithDescriptor:inputWeights:hiddenWeights:peepholeWeights:biases:gateActivations:outputResultActivation:"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @gateActivations@
gateActivationsSelector :: Selector
gateActivationsSelector = mkSelector "gateActivations"

-- | @Selector@ for @outputResultActivation@
outputResultActivationSelector :: Selector
outputResultActivationSelector = mkSelector "outputResultActivation"

-- | @Selector@ for @inputWeights@
inputWeightsSelector :: Selector
inputWeightsSelector = mkSelector "inputWeights"

-- | @Selector@ for @hiddenWeights@
hiddenWeightsSelector :: Selector
hiddenWeightsSelector = mkSelector "hiddenWeights"

-- | @Selector@ for @peepholeWeights@
peepholeWeightsSelector :: Selector
peepholeWeightsSelector = mkSelector "peepholeWeights"

-- | @Selector@ for @biases@
biasesSelector :: Selector
biasesSelector = mkSelector "biases"

-- | @Selector@ for @inputWeightsParameters@
inputWeightsParametersSelector :: Selector
inputWeightsParametersSelector = mkSelector "inputWeightsParameters"

-- | @Selector@ for @hiddenWeightsParameters@
hiddenWeightsParametersSelector :: Selector
hiddenWeightsParametersSelector = mkSelector "hiddenWeightsParameters"

-- | @Selector@ for @peepholeWeightsParameters@
peepholeWeightsParametersSelector :: Selector
peepholeWeightsParametersSelector = mkSelector "peepholeWeightsParameters"

-- | @Selector@ for @biasesParameters@
biasesParametersSelector :: Selector
biasesParametersSelector = mkSelector "biasesParameters"

