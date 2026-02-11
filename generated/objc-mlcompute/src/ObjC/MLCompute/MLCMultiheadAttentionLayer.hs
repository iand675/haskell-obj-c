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
  , layerWithDescriptor_weights_biases_attentionBiasesSelector
  , descriptorSelector
  , weightsSelector
  , biasesSelector
  , attentionBiasesSelector
  , weightsParametersSelector
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
    withObjCPtr descriptor $ \raw_descriptor ->
      withObjCPtr weights $ \raw_weights ->
        withObjCPtr biases $ \raw_biases ->
          withObjCPtr attentionBiases $ \raw_attentionBiases ->
            sendClassMsg cls' (mkSelector "layerWithDescriptor:weights:biases:attentionBiases:") (retPtr retVoid) [argPtr (castPtr raw_descriptor :: Ptr ()), argPtr (castPtr raw_weights :: Ptr ()), argPtr (castPtr raw_biases :: Ptr ()), argPtr (castPtr raw_attentionBiases :: Ptr ())] >>= retainedObject . castPtr

-- | descriptor
--
-- The multi-head attention descriptor
--
-- ObjC selector: @- descriptor@
descriptor :: IsMLCMultiheadAttentionLayer mlcMultiheadAttentionLayer => mlcMultiheadAttentionLayer -> IO (Id MLCMultiheadAttentionDescriptor)
descriptor mlcMultiheadAttentionLayer  =
  sendMsg mlcMultiheadAttentionLayer (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | weights
--
-- The weights of query, key, value and output projections
--
-- ObjC selector: @- weights@
weights :: IsMLCMultiheadAttentionLayer mlcMultiheadAttentionLayer => mlcMultiheadAttentionLayer -> IO (Id NSArray)
weights mlcMultiheadAttentionLayer  =
  sendMsg mlcMultiheadAttentionLayer (mkSelector "weights") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | biases
--
-- The biases of query, key, value and output projections
--
-- ObjC selector: @- biases@
biases :: IsMLCMultiheadAttentionLayer mlcMultiheadAttentionLayer => mlcMultiheadAttentionLayer -> IO (Id NSArray)
biases mlcMultiheadAttentionLayer  =
  sendMsg mlcMultiheadAttentionLayer (mkSelector "biases") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | attentionBiases
--
-- The biases added to key and value
--
-- ObjC selector: @- attentionBiases@
attentionBiases :: IsMLCMultiheadAttentionLayer mlcMultiheadAttentionLayer => mlcMultiheadAttentionLayer -> IO (Id NSArray)
attentionBiases mlcMultiheadAttentionLayer  =
  sendMsg mlcMultiheadAttentionLayer (mkSelector "attentionBiases") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | weightsParameters
--
-- The weights tensor parameters used for optimizer update
--
-- ObjC selector: @- weightsParameters@
weightsParameters :: IsMLCMultiheadAttentionLayer mlcMultiheadAttentionLayer => mlcMultiheadAttentionLayer -> IO (Id NSArray)
weightsParameters mlcMultiheadAttentionLayer  =
  sendMsg mlcMultiheadAttentionLayer (mkSelector "weightsParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | biasesParameters
--
-- The biases tensor parameters used for optimizer update
--
-- ObjC selector: @- biasesParameters@
biasesParameters :: IsMLCMultiheadAttentionLayer mlcMultiheadAttentionLayer => mlcMultiheadAttentionLayer -> IO (Id NSArray)
biasesParameters mlcMultiheadAttentionLayer  =
  sendMsg mlcMultiheadAttentionLayer (mkSelector "biasesParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithDescriptor:weights:biases:attentionBiases:@
layerWithDescriptor_weights_biases_attentionBiasesSelector :: Selector
layerWithDescriptor_weights_biases_attentionBiasesSelector = mkSelector "layerWithDescriptor:weights:biases:attentionBiases:"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @weights@
weightsSelector :: Selector
weightsSelector = mkSelector "weights"

-- | @Selector@ for @biases@
biasesSelector :: Selector
biasesSelector = mkSelector "biases"

-- | @Selector@ for @attentionBiases@
attentionBiasesSelector :: Selector
attentionBiasesSelector = mkSelector "attentionBiases"

-- | @Selector@ for @weightsParameters@
weightsParametersSelector :: Selector
weightsParametersSelector = mkSelector "weightsParameters"

-- | @Selector@ for @biasesParameters@
biasesParametersSelector :: Selector
biasesParametersSelector = mkSelector "biasesParameters"

