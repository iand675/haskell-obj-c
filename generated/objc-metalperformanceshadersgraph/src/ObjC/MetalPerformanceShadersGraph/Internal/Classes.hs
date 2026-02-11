{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.MetalPerformanceShadersGraph.Internal.Classes (
    module ObjC.MetalPerformanceShadersGraph.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.MetalPerformanceShaders.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Classes

-- ---------- MPSGraphObject ----------

-- | The common base class for all Metal Performance Shaders Graph objects.
--
-- Only the child classes should be used.
-- 
-- Phantom type for @MPSGraphObject@.
data MPSGraphObject

instance IsObjCObject (Id MPSGraphObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphObject"

class IsNSObject a => IsMPSGraphObject a where
  toMPSGraphObject :: a -> Id MPSGraphObject

instance IsMPSGraphObject (Id MPSGraphObject) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphObject) where
  toNSObject = unsafeCastId

-- ---------- MPSGraph ----------

-- | The optimized representation of a compute graph of operations and tensors.
--
-- An MPSGraph is a symbolic representation of operations to be utilized to execute compute graphs on a device.
-- 
-- Phantom type for @MPSGraph@.
data MPSGraph

instance IsObjCObject (Id MPSGraph) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraph"

class IsMPSGraphObject a => IsMPSGraph a where
  toMPSGraph :: a -> Id MPSGraph

instance IsMPSGraph (Id MPSGraph) where
  toMPSGraph = unsafeCastId

instance IsMPSGraphObject (Id MPSGraph) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraph) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphCompilationDescriptor ----------

-- | A class that consists of all the levers for compiling graphs.
-- 
-- Phantom type for @MPSGraphCompilationDescriptor@.
data MPSGraphCompilationDescriptor

instance IsObjCObject (Id MPSGraphCompilationDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphCompilationDescriptor"

class IsMPSGraphObject a => IsMPSGraphCompilationDescriptor a where
  toMPSGraphCompilationDescriptor :: a -> Id MPSGraphCompilationDescriptor

instance IsMPSGraphCompilationDescriptor (Id MPSGraphCompilationDescriptor) where
  toMPSGraphCompilationDescriptor = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphCompilationDescriptor) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphCompilationDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphConvolution2DOpDescriptor ----------

-- | A class that describes the properties of a 2D-convolution operator.
--
-- Use an instance of this class is to add a 2D-convolution operator with the desired properties to the graph.
-- 
-- Phantom type for @MPSGraphConvolution2DOpDescriptor@.
data MPSGraphConvolution2DOpDescriptor

instance IsObjCObject (Id MPSGraphConvolution2DOpDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphConvolution2DOpDescriptor"

class IsMPSGraphObject a => IsMPSGraphConvolution2DOpDescriptor a where
  toMPSGraphConvolution2DOpDescriptor :: a -> Id MPSGraphConvolution2DOpDescriptor

instance IsMPSGraphConvolution2DOpDescriptor (Id MPSGraphConvolution2DOpDescriptor) where
  toMPSGraphConvolution2DOpDescriptor = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphConvolution2DOpDescriptor) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphConvolution2DOpDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphConvolution3DOpDescriptor ----------

-- | A class that describes the properties of a 3D-convolution operator.
--
-- Use an instance of this class is to add a 3D-convolution operator with desired properties to the graph.
-- 
-- Phantom type for @MPSGraphConvolution3DOpDescriptor@.
data MPSGraphConvolution3DOpDescriptor

instance IsObjCObject (Id MPSGraphConvolution3DOpDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphConvolution3DOpDescriptor"

class IsMPSGraphObject a => IsMPSGraphConvolution3DOpDescriptor a where
  toMPSGraphConvolution3DOpDescriptor :: a -> Id MPSGraphConvolution3DOpDescriptor

instance IsMPSGraphConvolution3DOpDescriptor (Id MPSGraphConvolution3DOpDescriptor) where
  toMPSGraphConvolution3DOpDescriptor = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphConvolution3DOpDescriptor) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphConvolution3DOpDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphCreateSparseOpDescriptor ----------

-- | A class that describes the properties of a create sparse operation.
-- 
-- Phantom type for @MPSGraphCreateSparseOpDescriptor@.
data MPSGraphCreateSparseOpDescriptor

instance IsObjCObject (Id MPSGraphCreateSparseOpDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphCreateSparseOpDescriptor"

class IsMPSGraphObject a => IsMPSGraphCreateSparseOpDescriptor a where
  toMPSGraphCreateSparseOpDescriptor :: a -> Id MPSGraphCreateSparseOpDescriptor

instance IsMPSGraphCreateSparseOpDescriptor (Id MPSGraphCreateSparseOpDescriptor) where
  toMPSGraphCreateSparseOpDescriptor = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphCreateSparseOpDescriptor) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphCreateSparseOpDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphDepthwiseConvolution2DOpDescriptor ----------

-- | A class that defines the parameters for  a 2D-depthwise convolution operation.
--
-- An @MPSGraphDepthwiseConvolution2DOpDescriptor@ defines constant parameters for 2D-depthwise convolutions. Use this class with ``MPSGraph/depthwiseConvolution2DWithSourceTensor:weightsTensor:descriptor:name:``, ``MPSGraph/depthwiseConvolution2DDataGradientWithIncomingGradientTensor:weightsTensor:outputShape:descriptor:name:``, and ``MPSGraph/depthwiseConvolution2DWeightsGradientWithIncomingGradientTensor:sourceTensor:outputShape:descriptor:name:`` methods.
-- 
-- Phantom type for @MPSGraphDepthwiseConvolution2DOpDescriptor@.
data MPSGraphDepthwiseConvolution2DOpDescriptor

instance IsObjCObject (Id MPSGraphDepthwiseConvolution2DOpDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphDepthwiseConvolution2DOpDescriptor"

class IsMPSGraphObject a => IsMPSGraphDepthwiseConvolution2DOpDescriptor a where
  toMPSGraphDepthwiseConvolution2DOpDescriptor :: a -> Id MPSGraphDepthwiseConvolution2DOpDescriptor

instance IsMPSGraphDepthwiseConvolution2DOpDescriptor (Id MPSGraphDepthwiseConvolution2DOpDescriptor) where
  toMPSGraphDepthwiseConvolution2DOpDescriptor = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphDepthwiseConvolution2DOpDescriptor) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphDepthwiseConvolution2DOpDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphDepthwiseConvolution3DOpDescriptor ----------

-- | The class that defines the parameters for a 3D-depthwise convolution operation.
--
-- A @MPSGraphDepthwiseConvolution3DOpDescriptor@ defines constant parameters for 3D depthwise convolutions. Use this class with ``MPSGraph/depthwiseConvolution3DWithSourceTensor:weightsTensor:descriptor:name:``, ``MPSGraph/depthwiseConvolution3DDataGradientWithIncomingGradientTensor:weightsTensor:outputShape:descriptor:name:`` and ``MPSGraph/depthwiseConvolution3DWeightsGradientWithIncomingGradientTensor:sourceTensor:outputShape:descriptor:name:`` methods.
-- 
-- Phantom type for @MPSGraphDepthwiseConvolution3DOpDescriptor@.
data MPSGraphDepthwiseConvolution3DOpDescriptor

instance IsObjCObject (Id MPSGraphDepthwiseConvolution3DOpDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphDepthwiseConvolution3DOpDescriptor"

class IsMPSGraphObject a => IsMPSGraphDepthwiseConvolution3DOpDescriptor a where
  toMPSGraphDepthwiseConvolution3DOpDescriptor :: a -> Id MPSGraphDepthwiseConvolution3DOpDescriptor

instance IsMPSGraphDepthwiseConvolution3DOpDescriptor (Id MPSGraphDepthwiseConvolution3DOpDescriptor) where
  toMPSGraphDepthwiseConvolution3DOpDescriptor = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphDepthwiseConvolution3DOpDescriptor) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphDepthwiseConvolution3DOpDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphDevice ----------

-- | A class that describes the compute device.
-- 
-- Phantom type for @MPSGraphDevice@.
data MPSGraphDevice

instance IsObjCObject (Id MPSGraphDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphDevice"

class IsMPSGraphObject a => IsMPSGraphDevice a where
  toMPSGraphDevice :: a -> Id MPSGraphDevice

instance IsMPSGraphDevice (Id MPSGraphDevice) where
  toMPSGraphDevice = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphDevice) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphDevice) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphExecutable ----------

-- | The compiled representation of a compute graph executable.
--
-- An @MPSGraphExecutable@ is a compiled graph for specific feeds for specific target tensors and target operations.
-- 
-- Phantom type for @MPSGraphExecutable@.
data MPSGraphExecutable

instance IsObjCObject (Id MPSGraphExecutable) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphExecutable"

class IsMPSGraphObject a => IsMPSGraphExecutable a where
  toMPSGraphExecutable :: a -> Id MPSGraphExecutable

instance IsMPSGraphExecutable (Id MPSGraphExecutable) where
  toMPSGraphExecutable = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphExecutable) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphExecutable) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphExecutableExecutionDescriptor ----------

-- | A class that consists of all the levers  to synchronize and schedule executable execution.
-- 
-- Phantom type for @MPSGraphExecutableExecutionDescriptor@.
data MPSGraphExecutableExecutionDescriptor

instance IsObjCObject (Id MPSGraphExecutableExecutionDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphExecutableExecutionDescriptor"

class IsMPSGraphObject a => IsMPSGraphExecutableExecutionDescriptor a where
  toMPSGraphExecutableExecutionDescriptor :: a -> Id MPSGraphExecutableExecutionDescriptor

instance IsMPSGraphExecutableExecutionDescriptor (Id MPSGraphExecutableExecutionDescriptor) where
  toMPSGraphExecutableExecutionDescriptor = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphExecutableExecutionDescriptor) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphExecutableExecutionDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphExecutableSerializationDescriptor ----------

-- | A class that consists of all the levers  to serialize an executable.
-- 
-- Phantom type for @MPSGraphExecutableSerializationDescriptor@.
data MPSGraphExecutableSerializationDescriptor

instance IsObjCObject (Id MPSGraphExecutableSerializationDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphExecutableSerializationDescriptor"

class IsMPSGraphObject a => IsMPSGraphExecutableSerializationDescriptor a where
  toMPSGraphExecutableSerializationDescriptor :: a -> Id MPSGraphExecutableSerializationDescriptor

instance IsMPSGraphExecutableSerializationDescriptor (Id MPSGraphExecutableSerializationDescriptor) where
  toMPSGraphExecutableSerializationDescriptor = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphExecutableSerializationDescriptor) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphExecutableSerializationDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphExecutionDescriptor ----------

-- | A class that consists of all the levers  to synchronize and schedule graph execution.
-- 
-- Phantom type for @MPSGraphExecutionDescriptor@.
data MPSGraphExecutionDescriptor

instance IsObjCObject (Id MPSGraphExecutionDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphExecutionDescriptor"

class IsMPSGraphObject a => IsMPSGraphExecutionDescriptor a where
  toMPSGraphExecutionDescriptor :: a -> Id MPSGraphExecutionDescriptor

instance IsMPSGraphExecutionDescriptor (Id MPSGraphExecutionDescriptor) where
  toMPSGraphExecutionDescriptor = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphExecutionDescriptor) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphExecutionDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphFFTDescriptor ----------

-- | The class that defines the parameters for a fast Fourier transform (FFT) operation.
--
-- Use this descriptor with ``MPSGraph/fastFourierTransformWithTensor:axes:descriptor:name:``, ``MPSGraph/realToHermiteanFFTWithTensor:axesTensor:descriptor:name:``, and ``MPSGraph/HermiteanToRealFFTWithTensor:axesTensor:descriptor:name:`` methods.
-- 
-- Phantom type for @MPSGraphFFTDescriptor@.
data MPSGraphFFTDescriptor

instance IsObjCObject (Id MPSGraphFFTDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphFFTDescriptor"

class IsMPSGraphObject a => IsMPSGraphFFTDescriptor a where
  toMPSGraphFFTDescriptor :: a -> Id MPSGraphFFTDescriptor

instance IsMPSGraphFFTDescriptor (Id MPSGraphFFTDescriptor) where
  toMPSGraphFFTDescriptor = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphFFTDescriptor) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphFFTDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphGRUDescriptor ----------

-- | The class that defines the parameters for a gated recurrent unit (GRU) operation.
--
-- Use this descriptor with the following ``MPSGraph`` methods: - ``MPSGraph/GRUWithSourceTensor:recurrentWeight:inputWeight:bias:descriptor:name:`` - ``MPSGraph/GRUWithSourceTensor:recurrentWeight:inputWeight:bias:initState:descriptor:name:`` - ``MPSGraph/GRUWithSourceTensor:recurrentWeight:inputWeight:bias:initState:mask:secondaryBias:descriptor:name:`` - ``MPSGraph/GRUGradientsWithSourceTensor:recurrentWeight:sourceGradient:zState:outputFwd:inputWeight:bias:descriptor:name:`` - ``MPSGraph/GRUGradientsWithSourceTensor:recurrentWeight:sourceGradient:zState:outputFwd:inputWeight:bias:initState:descriptor:name:`` - ``MPSGraph/GRUGradientsWithSourceTensor:recurrentWeight:sourceGradient:zState:outputFwd:stateGradient:inputWeight:bias:initState:mask:secondaryBias:descriptor:name:``
-- 
-- Phantom type for @MPSGraphGRUDescriptor@.
data MPSGraphGRUDescriptor

instance IsObjCObject (Id MPSGraphGRUDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphGRUDescriptor"

class IsMPSGraphObject a => IsMPSGraphGRUDescriptor a where
  toMPSGraphGRUDescriptor :: a -> Id MPSGraphGRUDescriptor

instance IsMPSGraphGRUDescriptor (Id MPSGraphGRUDescriptor) where
  toMPSGraphGRUDescriptor = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphGRUDescriptor) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphGRUDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphImToColOpDescriptor ----------

-- | The class that defines the parameters for an image to column or column to image operation.
--
-- Use this descriptor with the following ``MPSGraph`` methods: - ``MPSGraph/imToColWithSourceTensor:descriptor:name:`` - ``MPSGraph/colToImWithSourceTensor:outputShape:descriptor:name:``
-- 
-- Phantom type for @MPSGraphImToColOpDescriptor@.
data MPSGraphImToColOpDescriptor

instance IsObjCObject (Id MPSGraphImToColOpDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphImToColOpDescriptor"

class IsMPSGraphObject a => IsMPSGraphImToColOpDescriptor a where
  toMPSGraphImToColOpDescriptor :: a -> Id MPSGraphImToColOpDescriptor

instance IsMPSGraphImToColOpDescriptor (Id MPSGraphImToColOpDescriptor) where
  toMPSGraphImToColOpDescriptor = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphImToColOpDescriptor) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphImToColOpDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphLSTMDescriptor ----------

-- | The class that defines the parameters for a long short-term memory (LSTM) operation.
--
-- Use this descriptor with the following ``MPSGraph`` methods: - ``MPSGraph/LSTMWithSourceTensor:recurrentWeight:initState:initCell:descriptor:name:`` - ``MPSGraph/LSTMWithSourceTensor:recurrentWeight:inputWeight:bias:initState:initCell:descriptor:name:`` - ``MPSGraph/LSTMWithSourceTensor:recurrentWeight:inputWeight:bias:initState:initCell:mask:peephole:descriptor:name:`` - ``MPSGraph/LSTMGradientsWithSourceTensor:recurrentWeight:sourceGradient:zState:cellOutputFwd:descriptor:name:`` - ``MPSGraph/LSTMGradientsWithSourceTensor:recurrentWeight:sourceGradient:zState:cellOutputFwd:inputWeight:bias:initState:initCell:descriptor:name:`` - ``MPSGraph/LSTMGradientsWithSourceTensor:recurrentWeight:sourceGradient:zState:cellOutputFwd:inputWeight:bias:initState:initCell:mask:descriptor:name:`` - ``MPSGraph/LSTMGradientsWithSourceTensor:recurrentWeight:sourceGradient:zState:cellOutputFwd:stateGradient:cellGradient:inputWeight:bias:initState:initCell:mask:peephole:descriptor:name:``
-- 
-- Phantom type for @MPSGraphLSTMDescriptor@.
data MPSGraphLSTMDescriptor

instance IsObjCObject (Id MPSGraphLSTMDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphLSTMDescriptor"

class IsMPSGraphObject a => IsMPSGraphLSTMDescriptor a where
  toMPSGraphLSTMDescriptor :: a -> Id MPSGraphLSTMDescriptor

instance IsMPSGraphLSTMDescriptor (Id MPSGraphLSTMDescriptor) where
  toMPSGraphLSTMDescriptor = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphLSTMDescriptor) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphLSTMDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphOperation ----------

-- | A symbolic representation of a compute operation.
--
-- @NSCopy@ will take a refrence, this is so @NSDictionary@ can work with the tensor. All operations are created, owned and destroyed by the graph.
-- 
-- Phantom type for @MPSGraphOperation@.
data MPSGraphOperation

instance IsObjCObject (Id MPSGraphOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphOperation"

class IsMPSGraphObject a => IsMPSGraphOperation a where
  toMPSGraphOperation :: a -> Id MPSGraphOperation

instance IsMPSGraphOperation (Id MPSGraphOperation) where
  toMPSGraphOperation = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphOperation) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphOperation) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphPooling2DOpDescriptor ----------

-- | The class that defines the parameters for a 2D pooling operation.
--
-- Use this descriptor with the following methods: - ``MPSGraph/maxPooling2DWithSourceTensor:descriptor:name:`` - ``MPSGraph/maxPooling2DReturnIndicesWithSourceTensor:descriptor:name:`` - ``MPSGraph/maxPooling2DGradientWithGradientTensor:sourceTensor:descriptor:name:`` - ``MPSGraph/maxPooling2DGradientWithGradientTensor:indicesTensor:outputShape:descriptor:name:`` - ``MPSGraph/maxPooling2DGradientWithGradientTensor:indicesTensor:outputShapeTensor:descriptor:name:`` - ``MPSGraph/avgPooling2DWithSourceTensor:descriptor:name:`` - ``MPSGraph/avgPooling2DGradientWithGradientTensor:sourceTensor:descriptor:name:``
-- 
-- Phantom type for @MPSGraphPooling2DOpDescriptor@.
data MPSGraphPooling2DOpDescriptor

instance IsObjCObject (Id MPSGraphPooling2DOpDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphPooling2DOpDescriptor"

class IsMPSGraphObject a => IsMPSGraphPooling2DOpDescriptor a where
  toMPSGraphPooling2DOpDescriptor :: a -> Id MPSGraphPooling2DOpDescriptor

instance IsMPSGraphPooling2DOpDescriptor (Id MPSGraphPooling2DOpDescriptor) where
  toMPSGraphPooling2DOpDescriptor = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphPooling2DOpDescriptor) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphPooling2DOpDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphPooling4DOpDescriptor ----------

-- | The class that defines the parameters for a 4D pooling operation.
--
-- Use this descriptor with the following methods: - ``MPSGraph/maxPooling4DWithSourceTensor:descriptor:name:`` - ``MPSGraph/maxPooling4DReturnIndicesWithSourceTensor:descriptor:name:`` - ``MPSGraph/maxPooling4DGradientWithGradientTensor:sourceTensor:descriptor:name:`` - ``MPSGraph/maxPooling4DGradientWithGradientTensor:indicesTensor:outputShape:descriptor:name:`` - ``MPSGraph/maxPooling4DGradientWithGradientTensor:indicesTensor:outputShapeTensor:descriptor:name:`` - ``MPSGraph/avgPooling4DWithSourceTensor:descriptor:name:`` - ``MPSGraph/avgPooling4DGradientWithGradientTensor:sourceTensor:descriptor:name:`` - ``MPSGraph/L2NormPooling4DWithSourceTensor:descriptor:name:`` - ``MPSGraph/L2NormPooling4DGradientWithGradientTensor:sourceTensor:descriptor:name:``
-- 
-- Phantom type for @MPSGraphPooling4DOpDescriptor@.
data MPSGraphPooling4DOpDescriptor

instance IsObjCObject (Id MPSGraphPooling4DOpDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphPooling4DOpDescriptor"

class IsMPSGraphObject a => IsMPSGraphPooling4DOpDescriptor a where
  toMPSGraphPooling4DOpDescriptor :: a -> Id MPSGraphPooling4DOpDescriptor

instance IsMPSGraphPooling4DOpDescriptor (Id MPSGraphPooling4DOpDescriptor) where
  toMPSGraphPooling4DOpDescriptor = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphPooling4DOpDescriptor) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphPooling4DOpDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphRandomOpDescriptor ----------

-- | A class that describes the random operation.
-- 
-- Phantom type for @MPSGraphRandomOpDescriptor@.
data MPSGraphRandomOpDescriptor

instance IsObjCObject (Id MPSGraphRandomOpDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphRandomOpDescriptor"

class IsMPSGraphObject a => IsMPSGraphRandomOpDescriptor a where
  toMPSGraphRandomOpDescriptor :: a -> Id MPSGraphRandomOpDescriptor

instance IsMPSGraphRandomOpDescriptor (Id MPSGraphRandomOpDescriptor) where
  toMPSGraphRandomOpDescriptor = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphRandomOpDescriptor) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphRandomOpDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphSingleGateRNNDescriptor ----------

-- | The class that defines the parameters for a single gate RNN operation.
--
-- Use this descriptor with the following ``MPSGraph`` methods: - ``MPSGraph/singleGateRNNWithSourceTensor:recurrentWeight:initState:descriptor:name:`` - ``MPSGraph/singleGateRNNWithSourceTensor:recurrentWeight:inputWeight:bias:initState:descriptor:name:`` - ``MPSGraph/singleGateRNNWithSourceTensor:recurrentWeight:inputWeight:bias:initState:mask:descriptor:name:`` - ``MPSGraph/singleGateRNNGradientsWithSourceTensor:recurrentWeight:sourceGradient:zState:initState:descriptor:name:`` - ``MPSGraph/singleGateRNNGradientsWithSourceTensor:recurrentWeight:sourceGradient:zState:inputWeight:bias:initState:descriptor:name:`` - ``MPSGraph/singleGateRNNGradientsWithSourceTensor:recurrentWeight:sourceGradient:zState:inputWeight:bias:initState:mask:descriptor:name:`` - ``MPSGraph/singleGateRNNGradientsWithSourceTensor:recurrentWeight:sourceGradient:zState:stateGradient:inputWeight:bias:initState:mask:descriptor:name:``
-- 
-- Phantom type for @MPSGraphSingleGateRNNDescriptor@.
data MPSGraphSingleGateRNNDescriptor

instance IsObjCObject (Id MPSGraphSingleGateRNNDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphSingleGateRNNDescriptor"

class IsMPSGraphObject a => IsMPSGraphSingleGateRNNDescriptor a where
  toMPSGraphSingleGateRNNDescriptor :: a -> Id MPSGraphSingleGateRNNDescriptor

instance IsMPSGraphSingleGateRNNDescriptor (Id MPSGraphSingleGateRNNDescriptor) where
  toMPSGraphSingleGateRNNDescriptor = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphSingleGateRNNDescriptor) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphSingleGateRNNDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphStencilOpDescriptor ----------

-- | The class that defines the parameters for a stencil operation.
--
-- Use this descriptor with the following ``MPSGraph`` method: - ``MPSGraph/stencilWithSourceTensor:weightsTensor:descriptor:name:``
-- 
-- Phantom type for @MPSGraphStencilOpDescriptor@.
data MPSGraphStencilOpDescriptor

instance IsObjCObject (Id MPSGraphStencilOpDescriptor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphStencilOpDescriptor"

class IsMPSGraphObject a => IsMPSGraphStencilOpDescriptor a where
  toMPSGraphStencilOpDescriptor :: a -> Id MPSGraphStencilOpDescriptor

instance IsMPSGraphStencilOpDescriptor (Id MPSGraphStencilOpDescriptor) where
  toMPSGraphStencilOpDescriptor = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphStencilOpDescriptor) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphStencilOpDescriptor) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphTensor ----------

-- | The symbolic representation of a compute data type.
--
-- @NSCopy@ will take a refrence, this is so @NSDictionary@ can work with the tensor. All tensors are created, owned and destroyed by the MPSGraph
-- 
-- Phantom type for @MPSGraphTensor@.
data MPSGraphTensor

instance IsObjCObject (Id MPSGraphTensor) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphTensor"

class IsMPSGraphObject a => IsMPSGraphTensor a where
  toMPSGraphTensor :: a -> Id MPSGraphTensor

instance IsMPSGraphTensor (Id MPSGraphTensor) where
  toMPSGraphTensor = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphTensor) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphTensor) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphTensorData ----------

-- | The representation of a compute data type.
--
-- Pass data to a graph using a tensor data, a reference will be taken to your data and used just in time when the graph is run.
-- 
-- Phantom type for @MPSGraphTensorData@.
data MPSGraphTensorData

instance IsObjCObject (Id MPSGraphTensorData) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphTensorData"

class IsMPSGraphObject a => IsMPSGraphTensorData a where
  toMPSGraphTensorData :: a -> Id MPSGraphTensorData

instance IsMPSGraphTensorData (Id MPSGraphTensorData) where
  toMPSGraphTensorData = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphTensorData) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphTensorData) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphType ----------

-- | The base type class for types on tensors.
-- 
-- Phantom type for @MPSGraphType@.
data MPSGraphType

instance IsObjCObject (Id MPSGraphType) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphType"

class IsMPSGraphObject a => IsMPSGraphType a where
  toMPSGraphType :: a -> Id MPSGraphType

instance IsMPSGraphType (Id MPSGraphType) where
  toMPSGraphType = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphType) where
  toMPSGraphObject = unsafeCastId

instance IsNSObject (Id MPSGraphType) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphVariableOp ----------

-- | The class that defines the parameters for a variable.
-- 
-- Phantom type for @MPSGraphVariableOp@.
data MPSGraphVariableOp

instance IsObjCObject (Id MPSGraphVariableOp) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphVariableOp"

class IsMPSGraphOperation a => IsMPSGraphVariableOp a where
  toMPSGraphVariableOp :: a -> Id MPSGraphVariableOp

instance IsMPSGraphVariableOp (Id MPSGraphVariableOp) where
  toMPSGraphVariableOp = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphVariableOp) where
  toMPSGraphObject = unsafeCastId

instance IsMPSGraphOperation (Id MPSGraphVariableOp) where
  toMPSGraphOperation = unsafeCastId

instance IsNSObject (Id MPSGraphVariableOp) where
  toNSObject = unsafeCastId

-- ---------- MPSGraphShapedType ----------

-- | The shaped type class for types on tensors with a shape and data type.
-- 
-- Phantom type for @MPSGraphShapedType@.
data MPSGraphShapedType

instance IsObjCObject (Id MPSGraphShapedType) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MPSGraphShapedType"

class IsMPSGraphType a => IsMPSGraphShapedType a where
  toMPSGraphShapedType :: a -> Id MPSGraphShapedType

instance IsMPSGraphShapedType (Id MPSGraphShapedType) where
  toMPSGraphShapedType = unsafeCastId

instance IsMPSGraphObject (Id MPSGraphShapedType) where
  toMPSGraphObject = unsafeCastId

instance IsMPSGraphType (Id MPSGraphShapedType) where
  toMPSGraphType = unsafeCastId

instance IsNSObject (Id MPSGraphShapedType) where
  toNSObject = unsafeCastId
