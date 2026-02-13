{-# LANGUAGE DataKinds #-}
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
  , biasesParameterSelector
  , biasesSelector
  , descriptorSelector
  , layerWithWeights_biases_descriptorSelector
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
    sendClassMessage cls' layerWithWeights_biases_descriptorSelector (toMLCTensor weights) (toMLCTensor biases) (toMLCConvolutionDescriptor descriptor)

-- | descriptor
--
-- The convolution descriptor
--
-- ObjC selector: @- descriptor@
descriptor :: IsMLCFullyConnectedLayer mlcFullyConnectedLayer => mlcFullyConnectedLayer -> IO (Id MLCConvolutionDescriptor)
descriptor mlcFullyConnectedLayer =
  sendMessage mlcFullyConnectedLayer descriptorSelector

-- | weights
--
-- The weights tensor used by the convolution layer
--
-- ObjC selector: @- weights@
weights :: IsMLCFullyConnectedLayer mlcFullyConnectedLayer => mlcFullyConnectedLayer -> IO (Id MLCTensor)
weights mlcFullyConnectedLayer =
  sendMessage mlcFullyConnectedLayer weightsSelector

-- | biases
--
-- The bias tensor used by the convolution layer
--
-- ObjC selector: @- biases@
biases :: IsMLCFullyConnectedLayer mlcFullyConnectedLayer => mlcFullyConnectedLayer -> IO (Id MLCTensor)
biases mlcFullyConnectedLayer =
  sendMessage mlcFullyConnectedLayer biasesSelector

-- | weightsParameter
--
-- The weights tensor parameter used for optimizer update
--
-- ObjC selector: @- weightsParameter@
weightsParameter :: IsMLCFullyConnectedLayer mlcFullyConnectedLayer => mlcFullyConnectedLayer -> IO (Id MLCTensorParameter)
weightsParameter mlcFullyConnectedLayer =
  sendMessage mlcFullyConnectedLayer weightsParameterSelector

-- | biasesParameter
--
-- The bias tensor parameter used for optimizer update
--
-- ObjC selector: @- biasesParameter@
biasesParameter :: IsMLCFullyConnectedLayer mlcFullyConnectedLayer => mlcFullyConnectedLayer -> IO (Id MLCTensorParameter)
biasesParameter mlcFullyConnectedLayer =
  sendMessage mlcFullyConnectedLayer biasesParameterSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithWeights:biases:descriptor:@
layerWithWeights_biases_descriptorSelector :: Selector '[Id MLCTensor, Id MLCTensor, Id MLCConvolutionDescriptor] (Id MLCFullyConnectedLayer)
layerWithWeights_biases_descriptorSelector = mkSelector "layerWithWeights:biases:descriptor:"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id MLCConvolutionDescriptor)
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @weights@
weightsSelector :: Selector '[] (Id MLCTensor)
weightsSelector = mkSelector "weights"

-- | @Selector@ for @biases@
biasesSelector :: Selector '[] (Id MLCTensor)
biasesSelector = mkSelector "biases"

-- | @Selector@ for @weightsParameter@
weightsParameterSelector :: Selector '[] (Id MLCTensorParameter)
weightsParameterSelector = mkSelector "weightsParameter"

-- | @Selector@ for @biasesParameter@
biasesParameterSelector :: Selector '[] (Id MLCTensorParameter)
biasesParameterSelector = mkSelector "biasesParameter"

