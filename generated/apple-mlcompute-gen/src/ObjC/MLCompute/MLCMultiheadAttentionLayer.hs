{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCMultiheadAttentionLayer
--
-- A multi-head attention layer
--
-- A multi-head "Scaled Dot-Product Attention" layer which attends to one or more entries in the input key-value pairs                N=Batch, S=source length, L=target length, E = model(embedding) dimension, K = Key dimension, V = value                dimension H = headCount. The sources to this layer are of shapes: Query:(N,L,E), Key:(N,S,K), Value:(N,S,V),                KeyMask:(N,S), AttentionMask:(1,L,S) or (NxH,L,S). KeyMask and AttentionMask are optional and either, both                or none of them can be passed. KeyMask is of Boolean type and AttentionMask can be of Float or Boolean type.                Output is of shape:(N,L,E).                For details refer to: https://pytorch.org/docs/stable/nn.html#multiheadattention
--
-- Generated bindings for @MLCMultiheadAttentionLayer@.
module ObjC.MLCompute.MLCMultiheadAttentionLayer
  ( MLCMultiheadAttentionLayer
  , IsMLCMultiheadAttentionLayer(..)
  , layerWithDescriptor_weights_biases_attentionBiases
  , descriptor
  , weights
  , biases
  , attentionBiases
  , weightsParameters
  , biasesParameters
  , attentionBiasesSelector
  , biasesParametersSelector
  , biasesSelector
  , descriptorSelector
  , layerWithDescriptor_weights_biases_attentionBiasesSelector
  , weightsParametersSelector
  , weightsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a multi-head attention layer
--
-- @weights@ — weights corresponding to query, key, value and output projections for all heads
--
-- @biases@ — Optional, biases corresponding to query, key, value and output projections for all heads
--
-- @attentionBiases@ — Optional, An array of biases added to the key and value respectively
--
-- Returns: A new MultiheadAttention layer
--
-- ObjC selector: @+ layerWithDescriptor:weights:biases:attentionBiases:@
layerWithDescriptor_weights_biases_attentionBiases :: (IsMLCMultiheadAttentionDescriptor descriptor, IsNSArray weights, IsNSArray biases, IsNSArray attentionBiases) => descriptor -> weights -> biases -> attentionBiases -> IO (Id MLCMultiheadAttentionLayer)
layerWithDescriptor_weights_biases_attentionBiases descriptor weights biases attentionBiases =
  do
    cls' <- getRequiredClass "MLCMultiheadAttentionLayer"
    sendClassMessage cls' layerWithDescriptor_weights_biases_attentionBiasesSelector (toMLCMultiheadAttentionDescriptor descriptor) (toNSArray weights) (toNSArray biases) (toNSArray attentionBiases)

-- | descriptor
--
-- The multi-head attention descriptor
--
-- ObjC selector: @- descriptor@
descriptor :: IsMLCMultiheadAttentionLayer mlcMultiheadAttentionLayer => mlcMultiheadAttentionLayer -> IO (Id MLCMultiheadAttentionDescriptor)
descriptor mlcMultiheadAttentionLayer =
  sendMessage mlcMultiheadAttentionLayer descriptorSelector

-- | weights
--
-- The weights of query, key, value and output projections
--
-- ObjC selector: @- weights@
weights :: IsMLCMultiheadAttentionLayer mlcMultiheadAttentionLayer => mlcMultiheadAttentionLayer -> IO (Id NSArray)
weights mlcMultiheadAttentionLayer =
  sendMessage mlcMultiheadAttentionLayer weightsSelector

-- | biases
--
-- The biases of query, key, value and output projections
--
-- ObjC selector: @- biases@
biases :: IsMLCMultiheadAttentionLayer mlcMultiheadAttentionLayer => mlcMultiheadAttentionLayer -> IO (Id NSArray)
biases mlcMultiheadAttentionLayer =
  sendMessage mlcMultiheadAttentionLayer biasesSelector

-- | attentionBiases
--
-- The biases added to key and value
--
-- ObjC selector: @- attentionBiases@
attentionBiases :: IsMLCMultiheadAttentionLayer mlcMultiheadAttentionLayer => mlcMultiheadAttentionLayer -> IO (Id NSArray)
attentionBiases mlcMultiheadAttentionLayer =
  sendMessage mlcMultiheadAttentionLayer attentionBiasesSelector

-- | weightsParameters
--
-- The weights tensor parameters used for optimizer update
--
-- ObjC selector: @- weightsParameters@
weightsParameters :: IsMLCMultiheadAttentionLayer mlcMultiheadAttentionLayer => mlcMultiheadAttentionLayer -> IO (Id NSArray)
weightsParameters mlcMultiheadAttentionLayer =
  sendMessage mlcMultiheadAttentionLayer weightsParametersSelector

-- | biasesParameters
--
-- The biases tensor parameters used for optimizer update
--
-- ObjC selector: @- biasesParameters@
biasesParameters :: IsMLCMultiheadAttentionLayer mlcMultiheadAttentionLayer => mlcMultiheadAttentionLayer -> IO (Id NSArray)
biasesParameters mlcMultiheadAttentionLayer =
  sendMessage mlcMultiheadAttentionLayer biasesParametersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithDescriptor:weights:biases:attentionBiases:@
layerWithDescriptor_weights_biases_attentionBiasesSelector :: Selector '[Id MLCMultiheadAttentionDescriptor, Id NSArray, Id NSArray, Id NSArray] (Id MLCMultiheadAttentionLayer)
layerWithDescriptor_weights_biases_attentionBiasesSelector = mkSelector "layerWithDescriptor:weights:biases:attentionBiases:"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id MLCMultiheadAttentionDescriptor)
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @weights@
weightsSelector :: Selector '[] (Id NSArray)
weightsSelector = mkSelector "weights"

-- | @Selector@ for @biases@
biasesSelector :: Selector '[] (Id NSArray)
biasesSelector = mkSelector "biases"

-- | @Selector@ for @attentionBiases@
attentionBiasesSelector :: Selector '[] (Id NSArray)
attentionBiasesSelector = mkSelector "attentionBiases"

-- | @Selector@ for @weightsParameters@
weightsParametersSelector :: Selector '[] (Id NSArray)
weightsParametersSelector = mkSelector "weightsParameters"

-- | @Selector@ for @biasesParameters@
biasesParametersSelector :: Selector '[] (Id NSArray)
biasesParametersSelector = mkSelector "biasesParameters"

