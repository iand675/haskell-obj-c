{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCEmbeddingLayer
--
-- An embedding layer which stores the words embedding
--
-- For details refer to: https://pytorch.org/docs/stable/nn.html#embedding              Only supported on CPU and can only be used as the first layer in a graph. If needs to be used with another graph compiled for a GPU device,              a second graph containing the embedding layer can be created first. The result of this layer can then be fed as an input to the second graph              and respectively the gradient result of the first layer of the second graph can be passed to this graph for weight update.
--
-- Generated bindings for @MLCEmbeddingLayer@.
module ObjC.MLCompute.MLCEmbeddingLayer
  ( MLCEmbeddingLayer
  , IsMLCEmbeddingLayer(..)
  , layerWithDescriptor_weights
  , descriptor
  , weights
  , weightsParameter
  , descriptorSelector
  , layerWithDescriptor_weightsSelector
  , weightsParameterSelector
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

-- | @+ layerWithDescriptor:weights:@
layerWithDescriptor_weights :: (IsMLCEmbeddingDescriptor descriptor, IsMLCTensor weights) => descriptor -> weights -> IO (Id MLCEmbeddingLayer)
layerWithDescriptor_weights descriptor weights =
  do
    cls' <- getRequiredClass "MLCEmbeddingLayer"
    sendClassMessage cls' layerWithDescriptor_weightsSelector (toMLCEmbeddingDescriptor descriptor) (toMLCTensor weights)

-- | @- descriptor@
descriptor :: IsMLCEmbeddingLayer mlcEmbeddingLayer => mlcEmbeddingLayer -> IO (Id MLCEmbeddingDescriptor)
descriptor mlcEmbeddingLayer =
  sendMessage mlcEmbeddingLayer descriptorSelector

-- | weights
--
-- The array of word embeddings
--
-- ObjC selector: @- weights@
weights :: IsMLCEmbeddingLayer mlcEmbeddingLayer => mlcEmbeddingLayer -> IO (Id MLCTensor)
weights mlcEmbeddingLayer =
  sendMessage mlcEmbeddingLayer weightsSelector

-- | weightsParameter
--
-- The weights tensor parameter used for optimizer update
--
-- ObjC selector: @- weightsParameter@
weightsParameter :: IsMLCEmbeddingLayer mlcEmbeddingLayer => mlcEmbeddingLayer -> IO (Id MLCTensorParameter)
weightsParameter mlcEmbeddingLayer =
  sendMessage mlcEmbeddingLayer weightsParameterSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithDescriptor:weights:@
layerWithDescriptor_weightsSelector :: Selector '[Id MLCEmbeddingDescriptor, Id MLCTensor] (Id MLCEmbeddingLayer)
layerWithDescriptor_weightsSelector = mkSelector "layerWithDescriptor:weights:"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id MLCEmbeddingDescriptor)
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @weights@
weightsSelector :: Selector '[] (Id MLCTensor)
weightsSelector = mkSelector "weights"

-- | @Selector@ for @weightsParameter@
weightsParameterSelector :: Selector '[] (Id MLCTensorParameter)
weightsParameterSelector = mkSelector "weightsParameter"

