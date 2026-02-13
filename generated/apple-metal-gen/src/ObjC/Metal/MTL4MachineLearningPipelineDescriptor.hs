{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Description for a machine learning pipeline state.
--
-- Generated bindings for @MTL4MachineLearningPipelineDescriptor@.
module ObjC.Metal.MTL4MachineLearningPipelineDescriptor
  ( MTL4MachineLearningPipelineDescriptor
  , IsMTL4MachineLearningPipelineDescriptor(..)
  , setInputDimensions_atBufferIndex
  , setInputDimensions_withRange
  , inputDimensionsAtBufferIndex
  , reset
  , label
  , setLabel
  , machineLearningFunctionDescriptor
  , setMachineLearningFunctionDescriptor
  , inputDimensionsAtBufferIndexSelector
  , labelSelector
  , machineLearningFunctionDescriptorSelector
  , resetSelector
  , setInputDimensions_atBufferIndexSelector
  , setInputDimensions_withRangeSelector
  , setLabelSelector
  , setMachineLearningFunctionDescriptorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | Sets the dimension of an input tensor at a buffer index.
--
-- - Parameters:   - dimensions: the dimensions of the tensor.   - bufferIndex: Index of the tensor to modify.
--
-- ObjC selector: @- setInputDimensions:atBufferIndex:@
setInputDimensions_atBufferIndex :: (IsMTL4MachineLearningPipelineDescriptor mtL4MachineLearningPipelineDescriptor, IsMTLTensorExtents dimensions) => mtL4MachineLearningPipelineDescriptor -> dimensions -> CLong -> IO ()
setInputDimensions_atBufferIndex mtL4MachineLearningPipelineDescriptor dimensions bufferIndex =
  sendMessage mtL4MachineLearningPipelineDescriptor setInputDimensions_atBufferIndexSelector (toMTLTensorExtents dimensions) bufferIndex

-- | Sets the dimensions of multiple input tensors on a range of buffer bindings.
--
-- Use this method to specify the dimensions of multiple input tensors at a range of indices in a single call.
--
-- You can indicate that any tensors in the range have unspecified dimensions by providing @NSNull@ at the their corresponding index location in the array.
--
-- - Important: The range's length property needs to match the number of dimensions you provide. Specifically, @range.length@ needs to match @dimensions.count@.
--
-- - Parameters:   - dimensions: An array of tensor extents.   - range: The range of inputs of the @dimensions@ argument.   The range's @length@ needs to match the dimensions' @count@ property.
--
-- ObjC selector: @- setInputDimensions:withRange:@
setInputDimensions_withRange :: (IsMTL4MachineLearningPipelineDescriptor mtL4MachineLearningPipelineDescriptor, IsNSArray dimensions) => mtL4MachineLearningPipelineDescriptor -> dimensions -> NSRange -> IO ()
setInputDimensions_withRange mtL4MachineLearningPipelineDescriptor dimensions range =
  sendMessage mtL4MachineLearningPipelineDescriptor setInputDimensions_withRangeSelector (toNSArray dimensions) range

-- | Obtains the dimensions of the input tensor at @bufferIndex@ if set, @nil@ otherwise.
--
-- ObjC selector: @- inputDimensionsAtBufferIndex:@
inputDimensionsAtBufferIndex :: IsMTL4MachineLearningPipelineDescriptor mtL4MachineLearningPipelineDescriptor => mtL4MachineLearningPipelineDescriptor -> CLong -> IO (Id MTLTensorExtents)
inputDimensionsAtBufferIndex mtL4MachineLearningPipelineDescriptor bufferIndex =
  sendMessage mtL4MachineLearningPipelineDescriptor inputDimensionsAtBufferIndexSelector bufferIndex

-- | Resets the descriptor to its default values.
--
-- ObjC selector: @- reset@
reset :: IsMTL4MachineLearningPipelineDescriptor mtL4MachineLearningPipelineDescriptor => mtL4MachineLearningPipelineDescriptor -> IO ()
reset mtL4MachineLearningPipelineDescriptor =
  sendMessage mtL4MachineLearningPipelineDescriptor resetSelector

-- | Assigns an optional string that helps identify pipeline states you create from this descriptor.
--
-- ObjC selector: @- label@
label :: IsMTL4MachineLearningPipelineDescriptor mtL4MachineLearningPipelineDescriptor => mtL4MachineLearningPipelineDescriptor -> IO (Id NSString)
label mtL4MachineLearningPipelineDescriptor =
  sendMessage mtL4MachineLearningPipelineDescriptor labelSelector

-- | Assigns an optional string that helps identify pipeline states you create from this descriptor.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsMTL4MachineLearningPipelineDescriptor mtL4MachineLearningPipelineDescriptor, IsNSString value) => mtL4MachineLearningPipelineDescriptor -> value -> IO ()
setLabel mtL4MachineLearningPipelineDescriptor value =
  sendMessage mtL4MachineLearningPipelineDescriptor setLabelSelector (toNSString value)

-- | Assigns the function that the machine learning pipeline you create from this descriptor executes.
--
-- ObjC selector: @- machineLearningFunctionDescriptor@
machineLearningFunctionDescriptor :: IsMTL4MachineLearningPipelineDescriptor mtL4MachineLearningPipelineDescriptor => mtL4MachineLearningPipelineDescriptor -> IO (Id MTL4FunctionDescriptor)
machineLearningFunctionDescriptor mtL4MachineLearningPipelineDescriptor =
  sendMessage mtL4MachineLearningPipelineDescriptor machineLearningFunctionDescriptorSelector

-- | Assigns the function that the machine learning pipeline you create from this descriptor executes.
--
-- ObjC selector: @- setMachineLearningFunctionDescriptor:@
setMachineLearningFunctionDescriptor :: (IsMTL4MachineLearningPipelineDescriptor mtL4MachineLearningPipelineDescriptor, IsMTL4FunctionDescriptor value) => mtL4MachineLearningPipelineDescriptor -> value -> IO ()
setMachineLearningFunctionDescriptor mtL4MachineLearningPipelineDescriptor value =
  sendMessage mtL4MachineLearningPipelineDescriptor setMachineLearningFunctionDescriptorSelector (toMTL4FunctionDescriptor value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setInputDimensions:atBufferIndex:@
setInputDimensions_atBufferIndexSelector :: Selector '[Id MTLTensorExtents, CLong] ()
setInputDimensions_atBufferIndexSelector = mkSelector "setInputDimensions:atBufferIndex:"

-- | @Selector@ for @setInputDimensions:withRange:@
setInputDimensions_withRangeSelector :: Selector '[Id NSArray, NSRange] ()
setInputDimensions_withRangeSelector = mkSelector "setInputDimensions:withRange:"

-- | @Selector@ for @inputDimensionsAtBufferIndex:@
inputDimensionsAtBufferIndexSelector :: Selector '[CLong] (Id MTLTensorExtents)
inputDimensionsAtBufferIndexSelector = mkSelector "inputDimensionsAtBufferIndex:"

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @machineLearningFunctionDescriptor@
machineLearningFunctionDescriptorSelector :: Selector '[] (Id MTL4FunctionDescriptor)
machineLearningFunctionDescriptorSelector = mkSelector "machineLearningFunctionDescriptor"

-- | @Selector@ for @setMachineLearningFunctionDescriptor:@
setMachineLearningFunctionDescriptorSelector :: Selector '[Id MTL4FunctionDescriptor] ()
setMachineLearningFunctionDescriptorSelector = mkSelector "setMachineLearningFunctionDescriptor:"

