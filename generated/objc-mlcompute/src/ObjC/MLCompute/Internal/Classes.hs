{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.MLCompute.Internal.Classes (
    module ObjC.MLCompute.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- MLCActivationDescriptor ----------

-- | MLCActivationDescriptor
--
-- The MLCActivationDescriptor specifies a neuron descriptor.
-- 
-- Phantom type for @MLCActivationDescriptor@.
data MLCActivationDescriptor

instance IsObjCObject (Id MLCActivationDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCActivationDescriptor"

class IsNSObject a => IsMLCActivationDescriptor a where
  toMLCActivationDescriptor :: a -> Id MLCActivationDescriptor

instance IsMLCActivationDescriptor (Id MLCActivationDescriptor) where
  toMLCActivationDescriptor = unsafeCastId

instance IsNSObject (Id MLCActivationDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MLCConvolutionDescriptor ----------

-- | MLCConvolutionDescriptor
--
-- The MLCConvolutionDescriptor specifies a convolution descriptor
-- 
-- Phantom type for @MLCConvolutionDescriptor@.
data MLCConvolutionDescriptor

instance IsObjCObject (Id MLCConvolutionDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCConvolutionDescriptor"

class IsNSObject a => IsMLCConvolutionDescriptor a where
  toMLCConvolutionDescriptor :: a -> Id MLCConvolutionDescriptor

instance IsMLCConvolutionDescriptor (Id MLCConvolutionDescriptor) where
  toMLCConvolutionDescriptor = unsafeCastId

instance IsNSObject (Id MLCConvolutionDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MLCDevice ----------

-- | MLCDevice
--
-- A device that will be used to execute a neural network.                If a MLCdevice is created with multiple devices using the [devicesWithType:selectMultipleDvices], on configurations                where multiple GPUs are available such as on the Mac Pro, the framework may transparently schedule the execution                across multiple GPUs.  There are some requirements for a MLCDevice with multiple devices such as there can only be                one training and/or inference graph associated with this device.  If multiple graphs are used, they must be compiled using                MLCGraphCompilationOptionsLinkGraphs specified in compileOptions and the multiple graphs should be linked together                with linkWithGraphs.
-- 
-- Phantom type for @MLCDevice@.
data MLCDevice

instance IsObjCObject (Id MLCDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCDevice"

class IsNSObject a => IsMLCDevice a where
  toMLCDevice :: a -> Id MLCDevice

instance IsMLCDevice (Id MLCDevice) where
  toMLCDevice = unsafeCastId

instance IsNSObject (Id MLCDevice) where
  toNSObject = unsafeCastId

-- ---------- MLCEmbeddingDescriptor ----------

-- | MLCEmbeddingDescriptor
--
-- The MLCEmbeddingDescriptor specifies an embedding layer descriptor
-- 
-- Phantom type for @MLCEmbeddingDescriptor@.
data MLCEmbeddingDescriptor

instance IsObjCObject (Id MLCEmbeddingDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCEmbeddingDescriptor"

class IsNSObject a => IsMLCEmbeddingDescriptor a where
  toMLCEmbeddingDescriptor :: a -> Id MLCEmbeddingDescriptor

instance IsMLCEmbeddingDescriptor (Id MLCEmbeddingDescriptor) where
  toMLCEmbeddingDescriptor = unsafeCastId

instance IsNSObject (Id MLCEmbeddingDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MLCGraph ----------

-- | MLCGraph
--
-- A graph of layers that can be used to build a training or inference graph
-- 
-- Phantom type for @MLCGraph@.
data MLCGraph

instance IsObjCObject (Id MLCGraph) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCGraph"

class IsNSObject a => IsMLCGraph a where
  toMLCGraph :: a -> Id MLCGraph

instance IsMLCGraph (Id MLCGraph) where
  toMLCGraph = unsafeCastId

instance IsNSObject (Id MLCGraph) where
  toNSObject = unsafeCastId

-- ---------- MLCLSTMDescriptor ----------

-- | MLCLSTMDescriptor
--
-- The MLCLSTMDescriptor specifies a LSTM descriptor
-- 
-- Phantom type for @MLCLSTMDescriptor@.
data MLCLSTMDescriptor

instance IsObjCObject (Id MLCLSTMDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCLSTMDescriptor"

class IsNSObject a => IsMLCLSTMDescriptor a where
  toMLCLSTMDescriptor :: a -> Id MLCLSTMDescriptor

instance IsMLCLSTMDescriptor (Id MLCLSTMDescriptor) where
  toMLCLSTMDescriptor = unsafeCastId

instance IsNSObject (Id MLCLSTMDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MLCLayer ----------

-- | MLCLayer
--
-- The base class for all MLCompute layers
--
-- There are as many MLCLayer subclasses as there are MLCompute neural network layer objects. Make one of those.                This class defines an polymorphic interface for them.
-- 
-- Phantom type for @MLCLayer@.
data MLCLayer

instance IsObjCObject (Id MLCLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCLayer"

class IsNSObject a => IsMLCLayer a where
  toMLCLayer :: a -> Id MLCLayer

instance IsMLCLayer (Id MLCLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCLossDescriptor ----------

-- | MLCLossDescriptor
--
-- The MLCLossDescriptor specifies a loss filter descriptor.
-- 
-- Phantom type for @MLCLossDescriptor@.
data MLCLossDescriptor

instance IsObjCObject (Id MLCLossDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCLossDescriptor"

class IsNSObject a => IsMLCLossDescriptor a where
  toMLCLossDescriptor :: a -> Id MLCLossDescriptor

instance IsMLCLossDescriptor (Id MLCLossDescriptor) where
  toMLCLossDescriptor = unsafeCastId

instance IsNSObject (Id MLCLossDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MLCMatMulDescriptor ----------

-- | MLCMatMulDescriptor
--
-- The MLCMatMulDescriptor specifies a batched matrix multiplication descriptor
-- 
-- Phantom type for @MLCMatMulDescriptor@.
data MLCMatMulDescriptor

instance IsObjCObject (Id MLCMatMulDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCMatMulDescriptor"

class IsNSObject a => IsMLCMatMulDescriptor a where
  toMLCMatMulDescriptor :: a -> Id MLCMatMulDescriptor

instance IsMLCMatMulDescriptor (Id MLCMatMulDescriptor) where
  toMLCMatMulDescriptor = unsafeCastId

instance IsNSObject (Id MLCMatMulDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MLCMultiheadAttentionDescriptor ----------

-- | MLCMultiheadAttentionDescriptor
--
-- The MLCMultiheadAttentionDescriptor specifies a Multi-Head Attention descriptor
-- 
-- Phantom type for @MLCMultiheadAttentionDescriptor@.
data MLCMultiheadAttentionDescriptor

instance IsObjCObject (Id MLCMultiheadAttentionDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCMultiheadAttentionDescriptor"

class IsNSObject a => IsMLCMultiheadAttentionDescriptor a where
  toMLCMultiheadAttentionDescriptor :: a -> Id MLCMultiheadAttentionDescriptor

instance IsMLCMultiheadAttentionDescriptor (Id MLCMultiheadAttentionDescriptor) where
  toMLCMultiheadAttentionDescriptor = unsafeCastId

instance IsNSObject (Id MLCMultiheadAttentionDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MLCOptimizer ----------

-- | MLCOptimizer
--
-- The MLCOptimizer specifies a base optimizer.
-- 
-- Phantom type for @MLCOptimizer@.
data MLCOptimizer

instance IsObjCObject (Id MLCOptimizer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCOptimizer"

class IsNSObject a => IsMLCOptimizer a where
  toMLCOptimizer :: a -> Id MLCOptimizer

instance IsMLCOptimizer (Id MLCOptimizer) where
  toMLCOptimizer = unsafeCastId

instance IsNSObject (Id MLCOptimizer) where
  toNSObject = unsafeCastId

-- ---------- MLCOptimizerDescriptor ----------

-- | MLCOptimizerDescriptor
--
-- The MLCOptimizerDescriptor specifies an optimizer descriptor.
-- 
-- Phantom type for @MLCOptimizerDescriptor@.
data MLCOptimizerDescriptor

instance IsObjCObject (Id MLCOptimizerDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCOptimizerDescriptor"

class IsNSObject a => IsMLCOptimizerDescriptor a where
  toMLCOptimizerDescriptor :: a -> Id MLCOptimizerDescriptor

instance IsMLCOptimizerDescriptor (Id MLCOptimizerDescriptor) where
  toMLCOptimizerDescriptor = unsafeCastId

instance IsNSObject (Id MLCOptimizerDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MLCPlatform ----------

-- | MLCPlatform
--
-- Utility class to set MLCompute global properties
-- 
-- Phantom type for @MLCPlatform@.
data MLCPlatform

instance IsObjCObject (Id MLCPlatform) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCPlatform"

class IsNSObject a => IsMLCPlatform a where
  toMLCPlatform :: a -> Id MLCPlatform

instance IsMLCPlatform (Id MLCPlatform) where
  toMLCPlatform = unsafeCastId

instance IsNSObject (Id MLCPlatform) where
  toNSObject = unsafeCastId

-- ---------- MLCPoolingDescriptor ----------

-- | MLCPoolingDescriptor
--
-- The MLCPoolingDescriptor specifies a pooling descriptor.
-- 
-- Phantom type for @MLCPoolingDescriptor@.
data MLCPoolingDescriptor

instance IsObjCObject (Id MLCPoolingDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCPoolingDescriptor"

class IsNSObject a => IsMLCPoolingDescriptor a where
  toMLCPoolingDescriptor :: a -> Id MLCPoolingDescriptor

instance IsMLCPoolingDescriptor (Id MLCPoolingDescriptor) where
  toMLCPoolingDescriptor = unsafeCastId

instance IsNSObject (Id MLCPoolingDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MLCTensor ----------

-- | MLCTensor
--
-- A tensor object
-- 
-- Phantom type for @MLCTensor@.
data MLCTensor

instance IsObjCObject (Id MLCTensor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCTensor"

class IsNSObject a => IsMLCTensor a where
  toMLCTensor :: a -> Id MLCTensor

instance IsMLCTensor (Id MLCTensor) where
  toMLCTensor = unsafeCastId

instance IsNSObject (Id MLCTensor) where
  toNSObject = unsafeCastId

-- ---------- MLCTensorData ----------

-- | MLCTensorData
--
-- An object to encapsulate memory to be used as tensor data
-- 
-- Phantom type for @MLCTensorData@.
data MLCTensorData

instance IsObjCObject (Id MLCTensorData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCTensorData"

class IsNSObject a => IsMLCTensorData a where
  toMLCTensorData :: a -> Id MLCTensorData

instance IsMLCTensorData (Id MLCTensorData) where
  toMLCTensorData = unsafeCastId

instance IsNSObject (Id MLCTensorData) where
  toNSObject = unsafeCastId

-- ---------- MLCTensorDescriptor ----------

-- | MLCTensorDescriptor
--
-- The MLCTensorDescriptor specifies a tensor descriptor.
-- 
-- Phantom type for @MLCTensorDescriptor@.
data MLCTensorDescriptor

instance IsObjCObject (Id MLCTensorDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCTensorDescriptor"

class IsNSObject a => IsMLCTensorDescriptor a where
  toMLCTensorDescriptor :: a -> Id MLCTensorDescriptor

instance IsMLCTensorDescriptor (Id MLCTensorDescriptor) where
  toMLCTensorDescriptor = unsafeCastId

instance IsNSObject (Id MLCTensorDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MLCTensorOptimizerDeviceData ----------

-- | Phantom type for @MLCTensorOptimizerDeviceData@.
data MLCTensorOptimizerDeviceData

instance IsObjCObject (Id MLCTensorOptimizerDeviceData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCTensorOptimizerDeviceData"

class IsNSObject a => IsMLCTensorOptimizerDeviceData a where
  toMLCTensorOptimizerDeviceData :: a -> Id MLCTensorOptimizerDeviceData

instance IsMLCTensorOptimizerDeviceData (Id MLCTensorOptimizerDeviceData) where
  toMLCTensorOptimizerDeviceData = unsafeCastId

instance IsNSObject (Id MLCTensorOptimizerDeviceData) where
  toNSObject = unsafeCastId

-- ---------- MLCTensorParameter ----------

-- | MLCTensorParameter
--
-- A tensor parameter object.  This is used to describe input tensors that are updated by the optimizer during training.
-- 
-- Phantom type for @MLCTensorParameter@.
data MLCTensorParameter

instance IsObjCObject (Id MLCTensorParameter) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCTensorParameter"

class IsNSObject a => IsMLCTensorParameter a where
  toMLCTensorParameter :: a -> Id MLCTensorParameter

instance IsMLCTensorParameter (Id MLCTensorParameter) where
  toMLCTensorParameter = unsafeCastId

instance IsNSObject (Id MLCTensorParameter) where
  toNSObject = unsafeCastId

-- ---------- MLCYOLOLossDescriptor ----------

-- | MLCYOLOLossDescriptor
--
-- The MLCYOLOLossDescriptor specifies a YOLO loss filter descriptor.
-- 
-- Phantom type for @MLCYOLOLossDescriptor@.
data MLCYOLOLossDescriptor

instance IsObjCObject (Id MLCYOLOLossDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCYOLOLossDescriptor"

class IsNSObject a => IsMLCYOLOLossDescriptor a where
  toMLCYOLOLossDescriptor :: a -> Id MLCYOLOLossDescriptor

instance IsMLCYOLOLossDescriptor (Id MLCYOLOLossDescriptor) where
  toMLCYOLOLossDescriptor = unsafeCastId

instance IsNSObject (Id MLCYOLOLossDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MLCInferenceGraph ----------

-- | MLCInferenceGraph
--
-- An inference graph created from one or more MLCGraph objects                plus additional layers added directly to the inference graph.
-- 
-- Phantom type for @MLCInferenceGraph@.
data MLCInferenceGraph

instance IsObjCObject (Id MLCInferenceGraph) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCInferenceGraph"

class IsMLCGraph a => IsMLCInferenceGraph a where
  toMLCInferenceGraph :: a -> Id MLCInferenceGraph

instance IsMLCInferenceGraph (Id MLCInferenceGraph) where
  toMLCInferenceGraph = unsafeCastId

instance IsMLCGraph (Id MLCInferenceGraph) where
  toMLCGraph = unsafeCastId

instance IsNSObject (Id MLCInferenceGraph) where
  toNSObject = unsafeCastId

-- ---------- MLCTrainingGraph ----------

-- | MLCTrainingGraph
--
-- A training graph created from one or more MLCGraph objects                plus additional layers added directly to the training graph.
-- 
-- Phantom type for @MLCTrainingGraph@.
data MLCTrainingGraph

instance IsObjCObject (Id MLCTrainingGraph) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCTrainingGraph"

class IsMLCGraph a => IsMLCTrainingGraph a where
  toMLCTrainingGraph :: a -> Id MLCTrainingGraph

instance IsMLCTrainingGraph (Id MLCTrainingGraph) where
  toMLCTrainingGraph = unsafeCastId

instance IsMLCGraph (Id MLCTrainingGraph) where
  toMLCGraph = unsafeCastId

instance IsNSObject (Id MLCTrainingGraph) where
  toNSObject = unsafeCastId

-- ---------- MLCActivationLayer ----------

-- | Phantom type for @MLCActivationLayer@.
data MLCActivationLayer

instance IsObjCObject (Id MLCActivationLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCActivationLayer"

class IsMLCLayer a => IsMLCActivationLayer a where
  toMLCActivationLayer :: a -> Id MLCActivationLayer

instance IsMLCActivationLayer (Id MLCActivationLayer) where
  toMLCActivationLayer = unsafeCastId

instance IsMLCLayer (Id MLCActivationLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCActivationLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCArithmeticLayer ----------

-- | MLCArithmeticLayer
--
-- An arithmetic layer
-- 
-- Phantom type for @MLCArithmeticLayer@.
data MLCArithmeticLayer

instance IsObjCObject (Id MLCArithmeticLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCArithmeticLayer"

class IsMLCLayer a => IsMLCArithmeticLayer a where
  toMLCArithmeticLayer :: a -> Id MLCArithmeticLayer

instance IsMLCArithmeticLayer (Id MLCArithmeticLayer) where
  toMLCArithmeticLayer = unsafeCastId

instance IsMLCLayer (Id MLCArithmeticLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCArithmeticLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCBatchNormalizationLayer ----------

-- | MLCBatchNormalizationLayer
--
-- A batch normalizaion layer
-- 
-- Phantom type for @MLCBatchNormalizationLayer@.
data MLCBatchNormalizationLayer

instance IsObjCObject (Id MLCBatchNormalizationLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCBatchNormalizationLayer"

class IsMLCLayer a => IsMLCBatchNormalizationLayer a where
  toMLCBatchNormalizationLayer :: a -> Id MLCBatchNormalizationLayer

instance IsMLCBatchNormalizationLayer (Id MLCBatchNormalizationLayer) where
  toMLCBatchNormalizationLayer = unsafeCastId

instance IsMLCLayer (Id MLCBatchNormalizationLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCBatchNormalizationLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCComparisonLayer ----------

-- | Compare layer.
--
-- The layer is used to perform element-wise comparison of two tensor. Returns a              tensor with the shape equal to the largest shape of operands and filled              with Boolean values result[i] = op1[i] ? op2[i], where ? corresponds to the              given @MLCComparisonOperation.@
-- 
-- Phantom type for @MLCComparisonLayer@.
data MLCComparisonLayer

instance IsObjCObject (Id MLCComparisonLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCComparisonLayer"

class IsMLCLayer a => IsMLCComparisonLayer a where
  toMLCComparisonLayer :: a -> Id MLCComparisonLayer

instance IsMLCComparisonLayer (Id MLCComparisonLayer) where
  toMLCComparisonLayer = unsafeCastId

instance IsMLCLayer (Id MLCComparisonLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCComparisonLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCConcatenationLayer ----------

-- | MLCConcatenationLayer
--
-- A concatenation layer
-- 
-- Phantom type for @MLCConcatenationLayer@.
data MLCConcatenationLayer

instance IsObjCObject (Id MLCConcatenationLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCConcatenationLayer"

class IsMLCLayer a => IsMLCConcatenationLayer a where
  toMLCConcatenationLayer :: a -> Id MLCConcatenationLayer

instance IsMLCConcatenationLayer (Id MLCConcatenationLayer) where
  toMLCConcatenationLayer = unsafeCastId

instance IsMLCLayer (Id MLCConcatenationLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCConcatenationLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCConvolutionLayer ----------

-- | MLCConvolutionLayer
--
-- A convolution layer
-- 
-- Phantom type for @MLCConvolutionLayer@.
data MLCConvolutionLayer

instance IsObjCObject (Id MLCConvolutionLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCConvolutionLayer"

class IsMLCLayer a => IsMLCConvolutionLayer a where
  toMLCConvolutionLayer :: a -> Id MLCConvolutionLayer

instance IsMLCConvolutionLayer (Id MLCConvolutionLayer) where
  toMLCConvolutionLayer = unsafeCastId

instance IsMLCLayer (Id MLCConvolutionLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCConvolutionLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCDropoutLayer ----------

-- | MLCDropoutLayer
--
-- A dropout layer
-- 
-- Phantom type for @MLCDropoutLayer@.
data MLCDropoutLayer

instance IsObjCObject (Id MLCDropoutLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCDropoutLayer"

class IsMLCLayer a => IsMLCDropoutLayer a where
  toMLCDropoutLayer :: a -> Id MLCDropoutLayer

instance IsMLCDropoutLayer (Id MLCDropoutLayer) where
  toMLCDropoutLayer = unsafeCastId

instance IsMLCLayer (Id MLCDropoutLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCDropoutLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCEmbeddingLayer ----------

-- | MLCEmbeddingLayer
--
-- An embedding layer which stores the words embedding
--
-- For details refer to: https://pytorch.org/docs/stable/nn.html#embedding              Only supported on CPU and can only be used as the first layer in a graph. If needs to be used with another graph compiled for a GPU device,              a second graph containing the embedding layer can be created first. The result of this layer can then be fed as an input to the second graph              and respectively the gradient result of the first layer of the second graph can be passed to this graph for weight update.
-- 
-- Phantom type for @MLCEmbeddingLayer@.
data MLCEmbeddingLayer

instance IsObjCObject (Id MLCEmbeddingLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCEmbeddingLayer"

class IsMLCLayer a => IsMLCEmbeddingLayer a where
  toMLCEmbeddingLayer :: a -> Id MLCEmbeddingLayer

instance IsMLCEmbeddingLayer (Id MLCEmbeddingLayer) where
  toMLCEmbeddingLayer = unsafeCastId

instance IsMLCLayer (Id MLCEmbeddingLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCEmbeddingLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCFullyConnectedLayer ----------

-- | MLCFullyConnectedLayer
--
-- A fully connected layer a.k.a a dense layer
--
-- For C:input feature channel, C':output feature channel, the layer maps (*,C) --> (*,C') where * can be 1, 2 or 3 dimesnion.                There is an exception for the case of (N,C,1,1) which gets mapped to (N,C',1,1).
-- 
-- Phantom type for @MLCFullyConnectedLayer@.
data MLCFullyConnectedLayer

instance IsObjCObject (Id MLCFullyConnectedLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCFullyConnectedLayer"

class IsMLCLayer a => IsMLCFullyConnectedLayer a where
  toMLCFullyConnectedLayer :: a -> Id MLCFullyConnectedLayer

instance IsMLCFullyConnectedLayer (Id MLCFullyConnectedLayer) where
  toMLCFullyConnectedLayer = unsafeCastId

instance IsMLCLayer (Id MLCFullyConnectedLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCFullyConnectedLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCGatherLayer ----------

-- | MLCGatherLayer
--
-- A gather layer
-- 
-- Phantom type for @MLCGatherLayer@.
data MLCGatherLayer

instance IsObjCObject (Id MLCGatherLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCGatherLayer"

class IsMLCLayer a => IsMLCGatherLayer a where
  toMLCGatherLayer :: a -> Id MLCGatherLayer

instance IsMLCGatherLayer (Id MLCGatherLayer) where
  toMLCGatherLayer = unsafeCastId

instance IsMLCLayer (Id MLCGatherLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCGatherLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCGramMatrixLayer ----------

-- | MLCGramMatrixLayer
--
-- A gram matrix layer
--
-- The MLComputeGramMatrix  specifies a layer which computes the uncentered cross-correlation                 values between the spatial planes of each feature channel of a tensor. If the input tensor batch is                 x = x[b, y, x, c], where 'b' is batch index, 'y' and 'x' are the spatial coordinates and 'c' is the feature channel                 index then this layer computes the values:
--
-- y = y[b, 1, f, c] = alpha * sum_{x,y} x[b,y,x,f] * x[b,y,x,c], where 'alpha' is a scaling factor.
--
-- This operation can be interpreted to be computing all combinations of fully connected layers                 between the different spatial planes of the input tensor. The results are stored in the feature channel and                 'x'-coordinate indices of the output batch.
--
-- The operation is performed independently for each tensor in a batch.
-- 
-- Phantom type for @MLCGramMatrixLayer@.
data MLCGramMatrixLayer

instance IsObjCObject (Id MLCGramMatrixLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCGramMatrixLayer"

class IsMLCLayer a => IsMLCGramMatrixLayer a where
  toMLCGramMatrixLayer :: a -> Id MLCGramMatrixLayer

instance IsMLCGramMatrixLayer (Id MLCGramMatrixLayer) where
  toMLCGramMatrixLayer = unsafeCastId

instance IsMLCLayer (Id MLCGramMatrixLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCGramMatrixLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCGroupNormalizationLayer ----------

-- | MLCGroupNormalizationLayer
--
-- A group normalizaion layer.  For more information, refer to https://pytorch.org/docs/stable/nn.html#groupnorm
-- 
-- Phantom type for @MLCGroupNormalizationLayer@.
data MLCGroupNormalizationLayer

instance IsObjCObject (Id MLCGroupNormalizationLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCGroupNormalizationLayer"

class IsMLCLayer a => IsMLCGroupNormalizationLayer a where
  toMLCGroupNormalizationLayer :: a -> Id MLCGroupNormalizationLayer

instance IsMLCGroupNormalizationLayer (Id MLCGroupNormalizationLayer) where
  toMLCGroupNormalizationLayer = unsafeCastId

instance IsMLCLayer (Id MLCGroupNormalizationLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCGroupNormalizationLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCInstanceNormalizationLayer ----------

-- | MLCInstanceNormalizationLayer
--
-- An instance normalization layer.  For more information refer to https://pytorch.org/docs/stable/nn.html#instancenorm2d
-- 
-- Phantom type for @MLCInstanceNormalizationLayer@.
data MLCInstanceNormalizationLayer

instance IsObjCObject (Id MLCInstanceNormalizationLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCInstanceNormalizationLayer"

class IsMLCLayer a => IsMLCInstanceNormalizationLayer a where
  toMLCInstanceNormalizationLayer :: a -> Id MLCInstanceNormalizationLayer

instance IsMLCInstanceNormalizationLayer (Id MLCInstanceNormalizationLayer) where
  toMLCInstanceNormalizationLayer = unsafeCastId

instance IsMLCLayer (Id MLCInstanceNormalizationLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCInstanceNormalizationLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCLSTMLayer ----------

-- | MLCLSTMLayer
--
-- A LSTM layer
--
-- The hidden and cell state for inputs and outputs have a layout of [numberOfLayers, numberOfDirections, batchSize, hiddenSize].
-- 
-- Phantom type for @MLCLSTMLayer@.
data MLCLSTMLayer

instance IsObjCObject (Id MLCLSTMLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCLSTMLayer"

class IsMLCLayer a => IsMLCLSTMLayer a where
  toMLCLSTMLayer :: a -> Id MLCLSTMLayer

instance IsMLCLSTMLayer (Id MLCLSTMLayer) where
  toMLCLSTMLayer = unsafeCastId

instance IsMLCLayer (Id MLCLSTMLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCLSTMLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCLayerNormalizationLayer ----------

-- | MLCLayerNormalizationLayer
--
-- The layer normalizaion layer.  For more information, refer to https://pytorch.org/docs/stable/nn.html#layernorm.
-- 
-- Phantom type for @MLCLayerNormalizationLayer@.
data MLCLayerNormalizationLayer

instance IsObjCObject (Id MLCLayerNormalizationLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCLayerNormalizationLayer"

class IsMLCLayer a => IsMLCLayerNormalizationLayer a where
  toMLCLayerNormalizationLayer :: a -> Id MLCLayerNormalizationLayer

instance IsMLCLayerNormalizationLayer (Id MLCLayerNormalizationLayer) where
  toMLCLayerNormalizationLayer = unsafeCastId

instance IsMLCLayer (Id MLCLayerNormalizationLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCLayerNormalizationLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCLossLayer ----------

-- | MLCLossLayer
--
-- A loss layer
-- 
-- Phantom type for @MLCLossLayer@.
data MLCLossLayer

instance IsObjCObject (Id MLCLossLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCLossLayer"

class IsMLCLayer a => IsMLCLossLayer a where
  toMLCLossLayer :: a -> Id MLCLossLayer

instance IsMLCLossLayer (Id MLCLossLayer) where
  toMLCLossLayer = unsafeCastId

instance IsMLCLayer (Id MLCLossLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCLossLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCMatMulLayer ----------

-- | MLCMatMulLayer
--
-- A batched matrix multiplication layer
-- 
-- Phantom type for @MLCMatMulLayer@.
data MLCMatMulLayer

instance IsObjCObject (Id MLCMatMulLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCMatMulLayer"

class IsMLCLayer a => IsMLCMatMulLayer a where
  toMLCMatMulLayer :: a -> Id MLCMatMulLayer

instance IsMLCMatMulLayer (Id MLCMatMulLayer) where
  toMLCMatMulLayer = unsafeCastId

instance IsMLCLayer (Id MLCMatMulLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCMatMulLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCMultiheadAttentionLayer ----------

-- | MLCMultiheadAttentionLayer
--
-- A multi-head attention layer
--
-- A multi-head "Scaled Dot-Product Attention" layer which attends to one or more entries in the input key-value pairs                N=Batch, S=source length, L=target length, E = model(embedding) dimension, K = Key dimension, V = value                dimension H = headCount. The sources to this layer are of shapes: Query:(N,L,E), Key:(N,S,K), Value:(N,S,V),                KeyMask:(N,S), AttentionMask:(1,L,S) or (NxH,L,S). KeyMask and AttentionMask are optional and either, both                or none of them can be passed. KeyMask is of Boolean type and AttentionMask can be of Float or Boolean type.                Output is of shape:(N,L,E).                For details refer to: https://pytorch.org/docs/stable/nn.html#multiheadattention
-- 
-- Phantom type for @MLCMultiheadAttentionLayer@.
data MLCMultiheadAttentionLayer

instance IsObjCObject (Id MLCMultiheadAttentionLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCMultiheadAttentionLayer"

class IsMLCLayer a => IsMLCMultiheadAttentionLayer a where
  toMLCMultiheadAttentionLayer :: a -> Id MLCMultiheadAttentionLayer

instance IsMLCMultiheadAttentionLayer (Id MLCMultiheadAttentionLayer) where
  toMLCMultiheadAttentionLayer = unsafeCastId

instance IsMLCLayer (Id MLCMultiheadAttentionLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCMultiheadAttentionLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCPaddingLayer ----------

-- | MLCPaddingLayer
--
-- A padding layer
-- 
-- Phantom type for @MLCPaddingLayer@.
data MLCPaddingLayer

instance IsObjCObject (Id MLCPaddingLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCPaddingLayer"

class IsMLCLayer a => IsMLCPaddingLayer a where
  toMLCPaddingLayer :: a -> Id MLCPaddingLayer

instance IsMLCPaddingLayer (Id MLCPaddingLayer) where
  toMLCPaddingLayer = unsafeCastId

instance IsMLCLayer (Id MLCPaddingLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCPaddingLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCPoolingLayer ----------

-- | MLCPoolingLayer
--
-- A pooling layer
-- 
-- Phantom type for @MLCPoolingLayer@.
data MLCPoolingLayer

instance IsObjCObject (Id MLCPoolingLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCPoolingLayer"

class IsMLCLayer a => IsMLCPoolingLayer a where
  toMLCPoolingLayer :: a -> Id MLCPoolingLayer

instance IsMLCPoolingLayer (Id MLCPoolingLayer) where
  toMLCPoolingLayer = unsafeCastId

instance IsMLCLayer (Id MLCPoolingLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCPoolingLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCReductionLayer ----------

-- | MLCReductionLayer
--
-- Reduce tensor values across a given dimension to a scalar value.
--
-- The layer is used to perform reductionType operation on a given dimension.                Result of this layer is a tensor of the same shape as source tensor,                except for the given dimension which is set to 1.
-- 
-- Phantom type for @MLCReductionLayer@.
data MLCReductionLayer

instance IsObjCObject (Id MLCReductionLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCReductionLayer"

class IsMLCLayer a => IsMLCReductionLayer a where
  toMLCReductionLayer :: a -> Id MLCReductionLayer

instance IsMLCReductionLayer (Id MLCReductionLayer) where
  toMLCReductionLayer = unsafeCastId

instance IsMLCLayer (Id MLCReductionLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCReductionLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCReshapeLayer ----------

-- | MLCReshapeLayer
--
-- A reshape layer.
-- 
-- Phantom type for @MLCReshapeLayer@.
data MLCReshapeLayer

instance IsObjCObject (Id MLCReshapeLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCReshapeLayer"

class IsMLCLayer a => IsMLCReshapeLayer a where
  toMLCReshapeLayer :: a -> Id MLCReshapeLayer

instance IsMLCReshapeLayer (Id MLCReshapeLayer) where
  toMLCReshapeLayer = unsafeCastId

instance IsMLCLayer (Id MLCReshapeLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCReshapeLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCScatterLayer ----------

-- | MLCScatterLayer
--
-- A scatter layer
-- 
-- Phantom type for @MLCScatterLayer@.
data MLCScatterLayer

instance IsObjCObject (Id MLCScatterLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCScatterLayer"

class IsMLCLayer a => IsMLCScatterLayer a where
  toMLCScatterLayer :: a -> Id MLCScatterLayer

instance IsMLCScatterLayer (Id MLCScatterLayer) where
  toMLCScatterLayer = unsafeCastId

instance IsMLCLayer (Id MLCScatterLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCScatterLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCSelectionLayer ----------

-- | Selection layer is used to select elements from two tensors
--
-- The selection layer takes a condition tensor which acts as a mask that chooses whether the corresponding element / row              in the output should be taken from tensor x (if the element in condition is true) or tensor y (if it is false).              The order of source tensors of the layer must be condition tensor, tensor x, and tensor y.
-- 
-- Phantom type for @MLCSelectionLayer@.
data MLCSelectionLayer

instance IsObjCObject (Id MLCSelectionLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCSelectionLayer"

class IsMLCLayer a => IsMLCSelectionLayer a where
  toMLCSelectionLayer :: a -> Id MLCSelectionLayer

instance IsMLCSelectionLayer (Id MLCSelectionLayer) where
  toMLCSelectionLayer = unsafeCastId

instance IsMLCLayer (Id MLCSelectionLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCSelectionLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCSliceLayer ----------

-- | Slice layer is used to slice a given source.
--
-- Slicing should not decrease the tensor dimension.              The start, end and stride vectors must have the same number of dimension as the source tensor.              Only positive stride is supported.
-- 
-- Phantom type for @MLCSliceLayer@.
data MLCSliceLayer

instance IsObjCObject (Id MLCSliceLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCSliceLayer"

class IsMLCLayer a => IsMLCSliceLayer a where
  toMLCSliceLayer :: a -> Id MLCSliceLayer

instance IsMLCSliceLayer (Id MLCSliceLayer) where
  toMLCSliceLayer = unsafeCastId

instance IsMLCLayer (Id MLCSliceLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCSliceLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCSoftmaxLayer ----------

-- | MLCSoftmaxLayer
--
-- A softmax layer
-- 
-- Phantom type for @MLCSoftmaxLayer@.
data MLCSoftmaxLayer

instance IsObjCObject (Id MLCSoftmaxLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCSoftmaxLayer"

class IsMLCLayer a => IsMLCSoftmaxLayer a where
  toMLCSoftmaxLayer :: a -> Id MLCSoftmaxLayer

instance IsMLCSoftmaxLayer (Id MLCSoftmaxLayer) where
  toMLCSoftmaxLayer = unsafeCastId

instance IsMLCLayer (Id MLCSoftmaxLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCSoftmaxLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCSplitLayer ----------

-- | MLCSplitLayer
--
-- A split layer
-- 
-- Phantom type for @MLCSplitLayer@.
data MLCSplitLayer

instance IsObjCObject (Id MLCSplitLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCSplitLayer"

class IsMLCLayer a => IsMLCSplitLayer a where
  toMLCSplitLayer :: a -> Id MLCSplitLayer

instance IsMLCSplitLayer (Id MLCSplitLayer) where
  toMLCSplitLayer = unsafeCastId

instance IsMLCLayer (Id MLCSplitLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCSplitLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCTransposeLayer ----------

-- | MLCTransposeLayer
--
-- A transpose layer
-- 
-- Phantom type for @MLCTransposeLayer@.
data MLCTransposeLayer

instance IsObjCObject (Id MLCTransposeLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCTransposeLayer"

class IsMLCLayer a => IsMLCTransposeLayer a where
  toMLCTransposeLayer :: a -> Id MLCTransposeLayer

instance IsMLCTransposeLayer (Id MLCTransposeLayer) where
  toMLCTransposeLayer = unsafeCastId

instance IsMLCLayer (Id MLCTransposeLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCTransposeLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCUpsampleLayer ----------

-- | MLCUpsampleLayer
--
-- An upsample layer
-- 
-- Phantom type for @MLCUpsampleLayer@.
data MLCUpsampleLayer

instance IsObjCObject (Id MLCUpsampleLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCUpsampleLayer"

class IsMLCLayer a => IsMLCUpsampleLayer a where
  toMLCUpsampleLayer :: a -> Id MLCUpsampleLayer

instance IsMLCUpsampleLayer (Id MLCUpsampleLayer) where
  toMLCUpsampleLayer = unsafeCastId

instance IsMLCLayer (Id MLCUpsampleLayer) where
  toMLCLayer = unsafeCastId

instance IsNSObject (Id MLCUpsampleLayer) where
  toNSObject = unsafeCastId

-- ---------- MLCAdamOptimizer ----------

-- | MLCAdamOptimizer
--
-- The MLCAdamOptimizer specifies the Adam optimizer.
-- 
-- Phantom type for @MLCAdamOptimizer@.
data MLCAdamOptimizer

instance IsObjCObject (Id MLCAdamOptimizer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCAdamOptimizer"

class IsMLCOptimizer a => IsMLCAdamOptimizer a where
  toMLCAdamOptimizer :: a -> Id MLCAdamOptimizer

instance IsMLCAdamOptimizer (Id MLCAdamOptimizer) where
  toMLCAdamOptimizer = unsafeCastId

instance IsMLCOptimizer (Id MLCAdamOptimizer) where
  toMLCOptimizer = unsafeCastId

instance IsNSObject (Id MLCAdamOptimizer) where
  toNSObject = unsafeCastId

-- ---------- MLCAdamWOptimizer ----------

-- | MLCAdamWOptimizer
--
-- The MLCAdamWOptimizer specifies the AdamW optimizer.
-- 
-- Phantom type for @MLCAdamWOptimizer@.
data MLCAdamWOptimizer

instance IsObjCObject (Id MLCAdamWOptimizer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCAdamWOptimizer"

class IsMLCOptimizer a => IsMLCAdamWOptimizer a where
  toMLCAdamWOptimizer :: a -> Id MLCAdamWOptimizer

instance IsMLCAdamWOptimizer (Id MLCAdamWOptimizer) where
  toMLCAdamWOptimizer = unsafeCastId

instance IsMLCOptimizer (Id MLCAdamWOptimizer) where
  toMLCOptimizer = unsafeCastId

instance IsNSObject (Id MLCAdamWOptimizer) where
  toNSObject = unsafeCastId

-- ---------- MLCRMSPropOptimizer ----------

-- | MLCRMSPropOptimizer
--
-- The MLCRMSPropOptimizer specifies the RMSProp optimizer.
-- 
-- Phantom type for @MLCRMSPropOptimizer@.
data MLCRMSPropOptimizer

instance IsObjCObject (Id MLCRMSPropOptimizer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCRMSPropOptimizer"

class IsMLCOptimizer a => IsMLCRMSPropOptimizer a where
  toMLCRMSPropOptimizer :: a -> Id MLCRMSPropOptimizer

instance IsMLCRMSPropOptimizer (Id MLCRMSPropOptimizer) where
  toMLCRMSPropOptimizer = unsafeCastId

instance IsMLCOptimizer (Id MLCRMSPropOptimizer) where
  toMLCOptimizer = unsafeCastId

instance IsNSObject (Id MLCRMSPropOptimizer) where
  toNSObject = unsafeCastId

-- ---------- MLCSGDOptimizer ----------

-- | MLCSGDOptimizer
--
-- The MLCSGDOptimizer specifies a stochastic gradient descent optimizer.
-- 
-- Phantom type for @MLCSGDOptimizer@.
data MLCSGDOptimizer

instance IsObjCObject (Id MLCSGDOptimizer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCSGDOptimizer"

class IsMLCOptimizer a => IsMLCSGDOptimizer a where
  toMLCSGDOptimizer :: a -> Id MLCSGDOptimizer

instance IsMLCSGDOptimizer (Id MLCSGDOptimizer) where
  toMLCSGDOptimizer = unsafeCastId

instance IsMLCOptimizer (Id MLCSGDOptimizer) where
  toMLCOptimizer = unsafeCastId

instance IsNSObject (Id MLCSGDOptimizer) where
  toNSObject = unsafeCastId

-- ---------- MLCYOLOLossLayer ----------

-- | MLCYOLOLossLayer
--
-- A YOLO loss layer
-- 
-- Phantom type for @MLCYOLOLossLayer@.
data MLCYOLOLossLayer

instance IsObjCObject (Id MLCYOLOLossLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCYOLOLossLayer"

class IsMLCLossLayer a => IsMLCYOLOLossLayer a where
  toMLCYOLOLossLayer :: a -> Id MLCYOLOLossLayer

instance IsMLCYOLOLossLayer (Id MLCYOLOLossLayer) where
  toMLCYOLOLossLayer = unsafeCastId

instance IsMLCLayer (Id MLCYOLOLossLayer) where
  toMLCLayer = unsafeCastId

instance IsMLCLossLayer (Id MLCYOLOLossLayer) where
  toMLCLossLayer = unsafeCastId

instance IsNSObject (Id MLCYOLOLossLayer) where
  toNSObject = unsafeCastId
