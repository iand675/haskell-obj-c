{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCConvolutionLayer
--
-- A convolution layer
--
-- Generated bindings for @MLCConvolutionLayer@.
module ObjC.MLCompute.MLCConvolutionLayer
  ( MLCConvolutionLayer
  , IsMLCConvolutionLayer(..)
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

-- | Create a convolution layer
--
-- @weights@ — The weights tensor
--
-- @biases@ — The bias tensor
--
-- @descriptor@ — The convolution descriptor
--
-- Returns: A new convolution layer.
--
-- ObjC selector: @+ layerWithWeights:biases:descriptor:@
layerWithWeights_biases_descriptor :: (IsMLCTensor weights, IsMLCTensor biases, IsMLCConvolutionDescriptor descriptor) => weights -> biases -> descriptor -> IO (Id MLCConvolutionLayer)
layerWithWeights_biases_descriptor weights biases descriptor =
  do
    cls' <- getRequiredClass "MLCConvolutionLayer"
    sendClassMessage cls' layerWithWeights_biases_descriptorSelector (toMLCTensor weights) (toMLCTensor biases) (toMLCConvolutionDescriptor descriptor)

-- | descriptor
--
-- The convolution descriptor
--
-- ObjC selector: @- descriptor@
descriptor :: IsMLCConvolutionLayer mlcConvolutionLayer => mlcConvolutionLayer -> IO (Id MLCConvolutionDescriptor)
descriptor mlcConvolutionLayer =
  sendMessage mlcConvolutionLayer descriptorSelector

-- | weights
--
-- The weights tensor used by the convolution layer
--
-- ObjC selector: @- weights@
weights :: IsMLCConvolutionLayer mlcConvolutionLayer => mlcConvolutionLayer -> IO (Id MLCTensor)
weights mlcConvolutionLayer =
  sendMessage mlcConvolutionLayer weightsSelector

-- | biases
--
-- The bias tensor used by the convolution layer
--
-- ObjC selector: @- biases@
biases :: IsMLCConvolutionLayer mlcConvolutionLayer => mlcConvolutionLayer -> IO (Id MLCTensor)
biases mlcConvolutionLayer =
  sendMessage mlcConvolutionLayer biasesSelector

-- | weightsParameter
--
-- The weights tensor parameter used for optimizer update
--
-- ObjC selector: @- weightsParameter@
weightsParameter :: IsMLCConvolutionLayer mlcConvolutionLayer => mlcConvolutionLayer -> IO (Id MLCTensorParameter)
weightsParameter mlcConvolutionLayer =
  sendMessage mlcConvolutionLayer weightsParameterSelector

-- | biasesParameter
--
-- The bias tensor parameter used for optimizer update
--
-- ObjC selector: @- biasesParameter@
biasesParameter :: IsMLCConvolutionLayer mlcConvolutionLayer => mlcConvolutionLayer -> IO (Id MLCTensorParameter)
biasesParameter mlcConvolutionLayer =
  sendMessage mlcConvolutionLayer biasesParameterSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithWeights:biases:descriptor:@
layerWithWeights_biases_descriptorSelector :: Selector '[Id MLCTensor, Id MLCTensor, Id MLCConvolutionDescriptor] (Id MLCConvolutionLayer)
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

