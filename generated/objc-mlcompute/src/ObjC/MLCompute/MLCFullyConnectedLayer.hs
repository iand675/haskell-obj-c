{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCFullyConnectedLayer
--
-- A fully connected layer a.k.a a dense layer
--
-- For C:input feature channel, C':output feature channel, the layer maps (*,C) --> (*,C') where * can be 1, 2 or 3 dimesnion.                There is an exception for the case of (N,C,1,1) which gets mapped to (N,C',1,1).
--
-- Generated bindings for @MLCFullyConnectedLayer@.
module ObjC.MLCompute.MLCFullyConnectedLayer
  ( MLCFullyConnectedLayer
  , IsMLCFullyConnectedLayer(..)
  , layerWithWeights_biases_descriptor
  , descriptor
  , weights
  , biases
  , weightsParameter
  , biasesParameter
  , layerWithWeights_biases_descriptorSelector
  , descriptorSelector
  , weightsSelector
  , biasesSelector
  , weightsParameterSelector
  , biasesParameterSelector


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

-- | Create a fully connected layer
--
-- @weights@ — The weights tensor
--
-- @biases@ — The bias tensor
--
-- @descriptor@ — The convolution descriptor
--
-- Returns: A new fully connected layer
--
-- ObjC selector: @+ layerWithWeights:biases:descriptor:@
layerWithWeights_biases_descriptor :: (IsMLCTensor weights, IsMLCTensor biases, IsMLCConvolutionDescriptor descriptor) => weights -> biases -> descriptor -> IO (Id MLCFullyConnectedLayer)
layerWithWeights_biases_descriptor weights biases descriptor =
  do
    cls' <- getRequiredClass "MLCFullyConnectedLayer"
    withObjCPtr weights $ \raw_weights ->
      withObjCPtr biases $ \raw_biases ->
        withObjCPtr descriptor $ \raw_descriptor ->
          sendClassMsg cls' (mkSelector "layerWithWeights:biases:descriptor:") (retPtr retVoid) [argPtr (castPtr raw_weights :: Ptr ()), argPtr (castPtr raw_biases :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= retainedObject . castPtr

-- | descriptor
--
-- The convolution descriptor
--
-- ObjC selector: @- descriptor@
descriptor :: IsMLCFullyConnectedLayer mlcFullyConnectedLayer => mlcFullyConnectedLayer -> IO (Id MLCConvolutionDescriptor)
descriptor mlcFullyConnectedLayer  =
  sendMsg mlcFullyConnectedLayer (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | weights
--
-- The weights tensor used by the convolution layer
--
-- ObjC selector: @- weights@
weights :: IsMLCFullyConnectedLayer mlcFullyConnectedLayer => mlcFullyConnectedLayer -> IO (Id MLCTensor)
weights mlcFullyConnectedLayer  =
  sendMsg mlcFullyConnectedLayer (mkSelector "weights") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | biases
--
-- The bias tensor used by the convolution layer
--
-- ObjC selector: @- biases@
biases :: IsMLCFullyConnectedLayer mlcFullyConnectedLayer => mlcFullyConnectedLayer -> IO (Id MLCTensor)
biases mlcFullyConnectedLayer  =
  sendMsg mlcFullyConnectedLayer (mkSelector "biases") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | weightsParameter
--
-- The weights tensor parameter used for optimizer update
--
-- ObjC selector: @- weightsParameter@
weightsParameter :: IsMLCFullyConnectedLayer mlcFullyConnectedLayer => mlcFullyConnectedLayer -> IO (Id MLCTensorParameter)
weightsParameter mlcFullyConnectedLayer  =
  sendMsg mlcFullyConnectedLayer (mkSelector "weightsParameter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | biasesParameter
--
-- The bias tensor parameter used for optimizer update
--
-- ObjC selector: @- biasesParameter@
biasesParameter :: IsMLCFullyConnectedLayer mlcFullyConnectedLayer => mlcFullyConnectedLayer -> IO (Id MLCTensorParameter)
biasesParameter mlcFullyConnectedLayer  =
  sendMsg mlcFullyConnectedLayer (mkSelector "biasesParameter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithWeights:biases:descriptor:@
layerWithWeights_biases_descriptorSelector :: Selector
layerWithWeights_biases_descriptorSelector = mkSelector "layerWithWeights:biases:descriptor:"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @weights@
weightsSelector :: Selector
weightsSelector = mkSelector "weights"

-- | @Selector@ for @biases@
biasesSelector :: Selector
biasesSelector = mkSelector "biases"

-- | @Selector@ for @weightsParameter@
weightsParameterSelector :: Selector
weightsParameterSelector = mkSelector "weightsParameter"

-- | @Selector@ for @biasesParameter@
biasesParameterSelector :: Selector
biasesParameterSelector = mkSelector "biasesParameter"

