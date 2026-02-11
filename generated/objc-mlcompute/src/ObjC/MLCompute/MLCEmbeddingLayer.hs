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
  , layerWithDescriptor_weightsSelector
  , descriptorSelector
  , weightsSelector
  , weightsParameterSelector


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

-- | @+ layerWithDescriptor:weights:@
layerWithDescriptor_weights :: (IsMLCEmbeddingDescriptor descriptor, IsMLCTensor weights) => descriptor -> weights -> IO (Id MLCEmbeddingLayer)
layerWithDescriptor_weights descriptor weights =
  do
    cls' <- getRequiredClass "MLCEmbeddingLayer"
    withObjCPtr descriptor $ \raw_descriptor ->
      withObjCPtr weights $ \raw_weights ->
        sendClassMsg cls' (mkSelector "layerWithDescriptor:weights:") (retPtr retVoid) [argPtr (castPtr raw_descriptor :: Ptr ()), argPtr (castPtr raw_weights :: Ptr ())] >>= retainedObject . castPtr

-- | @- descriptor@
descriptor :: IsMLCEmbeddingLayer mlcEmbeddingLayer => mlcEmbeddingLayer -> IO (Id MLCEmbeddingDescriptor)
descriptor mlcEmbeddingLayer  =
  sendMsg mlcEmbeddingLayer (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | weights
--
-- The array of word embeddings
--
-- ObjC selector: @- weights@
weights :: IsMLCEmbeddingLayer mlcEmbeddingLayer => mlcEmbeddingLayer -> IO (Id MLCTensor)
weights mlcEmbeddingLayer  =
  sendMsg mlcEmbeddingLayer (mkSelector "weights") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | weightsParameter
--
-- The weights tensor parameter used for optimizer update
--
-- ObjC selector: @- weightsParameter@
weightsParameter :: IsMLCEmbeddingLayer mlcEmbeddingLayer => mlcEmbeddingLayer -> IO (Id MLCTensorParameter)
weightsParameter mlcEmbeddingLayer  =
  sendMsg mlcEmbeddingLayer (mkSelector "weightsParameter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithDescriptor:weights:@
layerWithDescriptor_weightsSelector :: Selector
layerWithDescriptor_weightsSelector = mkSelector "layerWithDescriptor:weights:"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @weights@
weightsSelector :: Selector
weightsSelector = mkSelector "weights"

-- | @Selector@ for @weightsParameter@
weightsParameterSelector :: Selector
weightsParameterSelector = mkSelector "weightsParameter"

