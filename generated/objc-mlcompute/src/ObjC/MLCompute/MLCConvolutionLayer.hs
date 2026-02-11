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
    withObjCPtr weights $ \raw_weights ->
      withObjCPtr biases $ \raw_biases ->
        withObjCPtr descriptor $ \raw_descriptor ->
          sendClassMsg cls' (mkSelector "layerWithWeights:biases:descriptor:") (retPtr retVoid) [argPtr (castPtr raw_weights :: Ptr ()), argPtr (castPtr raw_biases :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= retainedObject . castPtr

-- | descriptor
--
-- The convolution descriptor
--
-- ObjC selector: @- descriptor@
descriptor :: IsMLCConvolutionLayer mlcConvolutionLayer => mlcConvolutionLayer -> IO (Id MLCConvolutionDescriptor)
descriptor mlcConvolutionLayer  =
  sendMsg mlcConvolutionLayer (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | weights
--
-- The weights tensor used by the convolution layer
--
-- ObjC selector: @- weights@
weights :: IsMLCConvolutionLayer mlcConvolutionLayer => mlcConvolutionLayer -> IO (Id MLCTensor)
weights mlcConvolutionLayer  =
  sendMsg mlcConvolutionLayer (mkSelector "weights") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | biases
--
-- The bias tensor used by the convolution layer
--
-- ObjC selector: @- biases@
biases :: IsMLCConvolutionLayer mlcConvolutionLayer => mlcConvolutionLayer -> IO (Id MLCTensor)
biases mlcConvolutionLayer  =
  sendMsg mlcConvolutionLayer (mkSelector "biases") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | weightsParameter
--
-- The weights tensor parameter used for optimizer update
--
-- ObjC selector: @- weightsParameter@
weightsParameter :: IsMLCConvolutionLayer mlcConvolutionLayer => mlcConvolutionLayer -> IO (Id MLCTensorParameter)
weightsParameter mlcConvolutionLayer  =
  sendMsg mlcConvolutionLayer (mkSelector "weightsParameter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | biasesParameter
--
-- The bias tensor parameter used for optimizer update
--
-- ObjC selector: @- biasesParameter@
biasesParameter :: IsMLCConvolutionLayer mlcConvolutionLayer => mlcConvolutionLayer -> IO (Id MLCTensorParameter)
biasesParameter mlcConvolutionLayer  =
  sendMsg mlcConvolutionLayer (mkSelector "biasesParameter") (retPtr retVoid) [] >>= retainedObject . castPtr

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

