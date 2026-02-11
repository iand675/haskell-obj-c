{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.CoreML.Internal.Classes (
    module ObjC.CoreML.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- MLArrayBatchProvider ----------

-- | A concrete convenience class conforming to MLBatchProvider.
-- 
-- Phantom type for @MLArrayBatchProvider@.
data MLArrayBatchProvider

instance IsObjCObject (Id MLArrayBatchProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLArrayBatchProvider"

class IsNSObject a => IsMLArrayBatchProvider a where
  toMLArrayBatchProvider :: a -> Id MLArrayBatchProvider

instance IsMLArrayBatchProvider (Id MLArrayBatchProvider) where
  toMLArrayBatchProvider = unsafeCastId

instance IsNSObject (Id MLArrayBatchProvider) where
  toNSObject = unsafeCastId

-- ---------- MLCPUComputeDevice ----------

-- | Represents a CPU compute device.
-- 
-- Phantom type for @MLCPUComputeDevice@.
data MLCPUComputeDevice

instance IsObjCObject (Id MLCPUComputeDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLCPUComputeDevice"

class IsNSObject a => IsMLCPUComputeDevice a where
  toMLCPUComputeDevice :: a -> Id MLCPUComputeDevice

instance IsMLCPUComputeDevice (Id MLCPUComputeDevice) where
  toMLCPUComputeDevice = unsafeCastId

instance IsNSObject (Id MLCPUComputeDevice) where
  toNSObject = unsafeCastId

-- ---------- MLComputePlan ----------

-- | A class describing the plan for executing a model.
--
-- The application can use the plan to estimate the necessary cost and resources of the model before running the predictions.
--
-- ``` // Load the compute plan of an ML Program model. [MLComputePlan loadContentsOfURL:modelURL configuration:configuration completionHandler:^(MLComputePlan * _Nullable computePlan, NSError * _Nullable error) {    if (!computePlan) {        // Handle error.        return;    }    MLModelStructureProgram *program = computePlan.modelStructure.program;    if (!program) {        [NSException raise:NSInternalInconsistencyException format:"Unexpected model type."];    }
--
-- MLModelStructureFunction *mainFunction = program.functions["main"];    if (!mainFunction) {        [NSException raise:NSInternalInconsistencyException format:"Missing main function."];    }
--
-- NSArray<MLModelStructureProgramOperation *> *operations = mainFunction.block.operations;    for (MLModelStructureProgramOperation *operation in operations) {        // Get the compute device usage for the operation.        MLComputeDeviceUsage *computeDeviceUsage = [computePlan computeDeviceUsageForMLProgramOperation:operation];        // Get the estimated cost of executing the operation.        MLComputePlanCost *estimatedCost = [computePlan estimatedCostOfMLProgramOperation:operation];
--
-- } }];```
-- 
-- Phantom type for @MLComputePlan@.
data MLComputePlan

instance IsObjCObject (Id MLComputePlan) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLComputePlan"

class IsNSObject a => IsMLComputePlan a where
  toMLComputePlan :: a -> Id MLComputePlan

instance IsMLComputePlan (Id MLComputePlan) where
  toMLComputePlan = unsafeCastId

instance IsNSObject (Id MLComputePlan) where
  toNSObject = unsafeCastId

-- ---------- MLComputePlanCost ----------

-- | A class representing the estimated cost of executing a layer/operation.
-- 
-- Phantom type for @MLComputePlanCost@.
data MLComputePlanCost

instance IsObjCObject (Id MLComputePlanCost) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLComputePlanCost"

class IsNSObject a => IsMLComputePlanCost a where
  toMLComputePlanCost :: a -> Id MLComputePlanCost

instance IsMLComputePlanCost (Id MLComputePlanCost) where
  toMLComputePlanCost = unsafeCastId

instance IsNSObject (Id MLComputePlanCost) where
  toNSObject = unsafeCastId

-- ---------- MLComputePlanDeviceUsage ----------

-- | The anticipated compute devices that would be used for executing a layer/operation.
-- 
-- Phantom type for @MLComputePlanDeviceUsage@.
data MLComputePlanDeviceUsage

instance IsObjCObject (Id MLComputePlanDeviceUsage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLComputePlanDeviceUsage"

class IsNSObject a => IsMLComputePlanDeviceUsage a where
  toMLComputePlanDeviceUsage :: a -> Id MLComputePlanDeviceUsage

instance IsMLComputePlanDeviceUsage (Id MLComputePlanDeviceUsage) where
  toMLComputePlanDeviceUsage = unsafeCastId

instance IsNSObject (Id MLComputePlanDeviceUsage) where
  toNSObject = unsafeCastId

-- ---------- MLDictionaryConstraint ----------

-- | MLDictionaryConstraint
--
-- Constraint describing expected NSDictionary properties
-- 
-- Phantom type for @MLDictionaryConstraint@.
data MLDictionaryConstraint

instance IsObjCObject (Id MLDictionaryConstraint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLDictionaryConstraint"

class IsNSObject a => IsMLDictionaryConstraint a where
  toMLDictionaryConstraint :: a -> Id MLDictionaryConstraint

instance IsMLDictionaryConstraint (Id MLDictionaryConstraint) where
  toMLDictionaryConstraint = unsafeCastId

instance IsNSObject (Id MLDictionaryConstraint) where
  toNSObject = unsafeCastId

-- ---------- MLDictionaryFeatureProvider ----------

-- | A concrete convenience class conforming to MLFeatureProvider.
-- 
-- Phantom type for @MLDictionaryFeatureProvider@.
data MLDictionaryFeatureProvider

instance IsObjCObject (Id MLDictionaryFeatureProvider) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLDictionaryFeatureProvider"

class IsNSObject a => IsMLDictionaryFeatureProvider a where
  toMLDictionaryFeatureProvider :: a -> Id MLDictionaryFeatureProvider

instance IsMLDictionaryFeatureProvider (Id MLDictionaryFeatureProvider) where
  toMLDictionaryFeatureProvider = unsafeCastId

instance IsNSObject (Id MLDictionaryFeatureProvider) where
  toNSObject = unsafeCastId

-- ---------- MLFeatureDescription ----------

-- | Description of a feature
-- 
-- Phantom type for @MLFeatureDescription@.
data MLFeatureDescription

instance IsObjCObject (Id MLFeatureDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLFeatureDescription"

class IsNSObject a => IsMLFeatureDescription a where
  toMLFeatureDescription :: a -> Id MLFeatureDescription

instance IsMLFeatureDescription (Id MLFeatureDescription) where
  toMLFeatureDescription = unsafeCastId

instance IsNSObject (Id MLFeatureDescription) where
  toNSObject = unsafeCastId

-- ---------- MLFeatureValue ----------

-- | An immutable variant holding a data value of a supported MLFeatureType
--
-- MLFeatureValue does not support type conversion in its accessor properties. It can also have a missing or undefined value of a well defined type.
-- 
-- Phantom type for @MLFeatureValue@.
data MLFeatureValue

instance IsObjCObject (Id MLFeatureValue) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLFeatureValue"

class IsNSObject a => IsMLFeatureValue a where
  toMLFeatureValue :: a -> Id MLFeatureValue

instance IsMLFeatureValue (Id MLFeatureValue) where
  toMLFeatureValue = unsafeCastId

instance IsNSObject (Id MLFeatureValue) where
  toNSObject = unsafeCastId

-- ---------- MLGPUComputeDevice ----------

-- | Represents a GPU compute device.
-- 
-- Phantom type for @MLGPUComputeDevice@.
data MLGPUComputeDevice

instance IsObjCObject (Id MLGPUComputeDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLGPUComputeDevice"

class IsNSObject a => IsMLGPUComputeDevice a where
  toMLGPUComputeDevice :: a -> Id MLGPUComputeDevice

instance IsMLGPUComputeDevice (Id MLGPUComputeDevice) where
  toMLGPUComputeDevice = unsafeCastId

instance IsNSObject (Id MLGPUComputeDevice) where
  toNSObject = unsafeCastId

-- ---------- MLImageConstraint ----------

-- | MLImageConstraint
--
-- Constraint on image properties.
-- 
-- Phantom type for @MLImageConstraint@.
data MLImageConstraint

instance IsObjCObject (Id MLImageConstraint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLImageConstraint"

class IsNSObject a => IsMLImageConstraint a where
  toMLImageConstraint :: a -> Id MLImageConstraint

instance IsMLImageConstraint (Id MLImageConstraint) where
  toMLImageConstraint = unsafeCastId

instance IsNSObject (Id MLImageConstraint) where
  toNSObject = unsafeCastId

-- ---------- MLImageSize ----------

-- | Phantom type for @MLImageSize@.
data MLImageSize

instance IsObjCObject (Id MLImageSize) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLImageSize"

class IsNSObject a => IsMLImageSize a where
  toMLImageSize :: a -> Id MLImageSize

instance IsMLImageSize (Id MLImageSize) where
  toMLImageSize = unsafeCastId

instance IsNSObject (Id MLImageSize) where
  toNSObject = unsafeCastId

-- ---------- MLImageSizeConstraint ----------

-- | Phantom type for @MLImageSizeConstraint@.
data MLImageSizeConstraint

instance IsObjCObject (Id MLImageSizeConstraint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLImageSizeConstraint"

class IsNSObject a => IsMLImageSizeConstraint a where
  toMLImageSizeConstraint :: a -> Id MLImageSizeConstraint

instance IsMLImageSizeConstraint (Id MLImageSizeConstraint) where
  toMLImageSizeConstraint = unsafeCastId

instance IsNSObject (Id MLImageSizeConstraint) where
  toNSObject = unsafeCastId

-- ---------- MLKey ----------

-- | A class representing key used to store any value against
-- 
-- Phantom type for @MLKey@.
data MLKey

instance IsObjCObject (Id MLKey) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLKey"

class IsNSObject a => IsMLKey a where
  toMLKey :: a -> Id MLKey

instance IsMLKey (Id MLKey) where
  toMLKey = unsafeCastId

instance IsNSObject (Id MLKey) where
  toNSObject = unsafeCastId

-- ---------- MLModel ----------

-- | MLModel
--
-- Construct a model and evaluate on a specific set of input features. Inputs and outputs are accessed via the MLFeatureProvider protocol. Returns a model or nil if there is an error.
-- 
-- Phantom type for @MLModel@.
data MLModel

instance IsObjCObject (Id MLModel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLModel"

class IsNSObject a => IsMLModel a where
  toMLModel :: a -> Id MLModel

instance IsMLModel (Id MLModel) where
  toMLModel = unsafeCastId

instance IsNSObject (Id MLModel) where
  toNSObject = unsafeCastId

-- ---------- MLModelAsset ----------

-- | A compiled model asset.
--
-- @MLModelAsset@ is an abstraction of a compiled model, which can be:
--
-- - @.mlmodelc@ bundle on the file system  - In-memory model specification
--
-- It provides the unified interface to query the model description and to instantiate @MLModel@.
--
-- ```swift // Creates an object. let modelAsset = MLModelAsset(url: modelURL)
--
-- // Query the model description let description = try await modelAsset.modelDescription
--
-- // Query the list of functions in the model asset. let functionNames = try await modelAsset.functionNames
--
-- // Query the model description of a specific function. let descriptionOfMyFunction = try await modelAsset.modelDescription(of: "MyFunction")
--
-- // Instantiate @MLModel@ for "MyFunction". let modelConfiguration = MLModelConfiguration() modelConfiguration.functionName = "MyFunction" let model = try await MLModel.load(asset: modelAsset, configuration: modelConfiguration) ```
-- 
-- Phantom type for @MLModelAsset@.
data MLModelAsset

instance IsObjCObject (Id MLModelAsset) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLModelAsset"

class IsNSObject a => IsMLModelAsset a where
  toMLModelAsset :: a -> Id MLModelAsset

instance IsMLModelAsset (Id MLModelAsset) where
  toMLModelAsset = unsafeCastId

instance IsNSObject (Id MLModelAsset) where
  toNSObject = unsafeCastId

-- ---------- MLModelCollection ----------

-- | MLModelCollection
--
-- A collection of models managed as part of Core ML Model Deployment.
-- 
-- Phantom type for @MLModelCollection@.
data MLModelCollection

instance IsObjCObject (Id MLModelCollection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLModelCollection"

class IsNSObject a => IsMLModelCollection a where
  toMLModelCollection :: a -> Id MLModelCollection

instance IsMLModelCollection (Id MLModelCollection) where
  toMLModelCollection = unsafeCastId

instance IsNSObject (Id MLModelCollection) where
  toNSObject = unsafeCastId

-- ---------- MLModelCollectionEntry ----------

-- | MLModelCollectionEntry Information about a model in a model collection.
-- 
-- Phantom type for @MLModelCollectionEntry@.
data MLModelCollectionEntry

instance IsObjCObject (Id MLModelCollectionEntry) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLModelCollectionEntry"

class IsNSObject a => IsMLModelCollectionEntry a where
  toMLModelCollectionEntry :: a -> Id MLModelCollectionEntry

instance IsMLModelCollectionEntry (Id MLModelCollectionEntry) where
  toMLModelCollectionEntry = unsafeCastId

instance IsNSObject (Id MLModelCollectionEntry) where
  toNSObject = unsafeCastId

-- ---------- MLModelConfiguration ----------

-- | An object to hold options for loading a model.
-- 
-- Phantom type for @MLModelConfiguration@.
data MLModelConfiguration

instance IsObjCObject (Id MLModelConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLModelConfiguration"

class IsNSObject a => IsMLModelConfiguration a where
  toMLModelConfiguration :: a -> Id MLModelConfiguration

instance IsMLModelConfiguration (Id MLModelConfiguration) where
  toMLModelConfiguration = unsafeCastId

instance IsNSObject (Id MLModelConfiguration) where
  toNSObject = unsafeCastId

-- ---------- MLModelDescription ----------

-- | A description of a model containing input, output, and state feature descriptions, optionally outputted features with special meaning and metadata.
-- 
-- Phantom type for @MLModelDescription@.
data MLModelDescription

instance IsObjCObject (Id MLModelDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLModelDescription"

class IsNSObject a => IsMLModelDescription a where
  toMLModelDescription :: a -> Id MLModelDescription

instance IsMLModelDescription (Id MLModelDescription) where
  toMLModelDescription = unsafeCastId

instance IsNSObject (Id MLModelDescription) where
  toNSObject = unsafeCastId

-- ---------- MLModelStructure ----------

-- | A class representing the structure of a model.
--
-- ``` // Load the model structure. [MLModelStructure loadContentsOfURL:modelURL completionHandler:^(MLModelStructure * _Nullable modelStructure, NSError * _Nullable error) {    if (!modelStructure) {        // Handle error.        return;    }    if (modelStructure.neuralNetwork) {        // Examine Neural network model.    } else if (modelStructure.program) {        // Examine ML Program model.    } else if (modelStructure.pipeline) {        // Examine Pipeline model.    } else {        // The model type is something else.    } }]; ```
-- 
-- Phantom type for @MLModelStructure@.
data MLModelStructure

instance IsObjCObject (Id MLModelStructure) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLModelStructure"

class IsNSObject a => IsMLModelStructure a where
  toMLModelStructure :: a -> Id MLModelStructure

instance IsMLModelStructure (Id MLModelStructure) where
  toMLModelStructure = unsafeCastId

instance IsNSObject (Id MLModelStructure) where
  toNSObject = unsafeCastId

-- ---------- MLModelStructureNeuralNetwork ----------

-- | A class representing the structure of a NeuralNetwork model.
-- 
-- Phantom type for @MLModelStructureNeuralNetwork@.
data MLModelStructureNeuralNetwork

instance IsObjCObject (Id MLModelStructureNeuralNetwork) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLModelStructureNeuralNetwork"

class IsNSObject a => IsMLModelStructureNeuralNetwork a where
  toMLModelStructureNeuralNetwork :: a -> Id MLModelStructureNeuralNetwork

instance IsMLModelStructureNeuralNetwork (Id MLModelStructureNeuralNetwork) where
  toMLModelStructureNeuralNetwork = unsafeCastId

instance IsNSObject (Id MLModelStructureNeuralNetwork) where
  toNSObject = unsafeCastId

-- ---------- MLModelStructureNeuralNetworkLayer ----------

-- | A class representing a layer in a NeuralNetwork.
-- 
-- Phantom type for @MLModelStructureNeuralNetworkLayer@.
data MLModelStructureNeuralNetworkLayer

instance IsObjCObject (Id MLModelStructureNeuralNetworkLayer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLModelStructureNeuralNetworkLayer"

class IsNSObject a => IsMLModelStructureNeuralNetworkLayer a where
  toMLModelStructureNeuralNetworkLayer :: a -> Id MLModelStructureNeuralNetworkLayer

instance IsMLModelStructureNeuralNetworkLayer (Id MLModelStructureNeuralNetworkLayer) where
  toMLModelStructureNeuralNetworkLayer = unsafeCastId

instance IsNSObject (Id MLModelStructureNeuralNetworkLayer) where
  toNSObject = unsafeCastId

-- ---------- MLModelStructurePipeline ----------

-- | A class representing the structure of a Pipeline model.
-- 
-- Phantom type for @MLModelStructurePipeline@.
data MLModelStructurePipeline

instance IsObjCObject (Id MLModelStructurePipeline) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLModelStructurePipeline"

class IsNSObject a => IsMLModelStructurePipeline a where
  toMLModelStructurePipeline :: a -> Id MLModelStructurePipeline

instance IsMLModelStructurePipeline (Id MLModelStructurePipeline) where
  toMLModelStructurePipeline = unsafeCastId

instance IsNSObject (Id MLModelStructurePipeline) where
  toNSObject = unsafeCastId

-- ---------- MLModelStructureProgram ----------

-- | A class representing the structure of an ML Program model.
-- 
-- Phantom type for @MLModelStructureProgram@.
data MLModelStructureProgram

instance IsObjCObject (Id MLModelStructureProgram) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLModelStructureProgram"

class IsNSObject a => IsMLModelStructureProgram a where
  toMLModelStructureProgram :: a -> Id MLModelStructureProgram

instance IsMLModelStructureProgram (Id MLModelStructureProgram) where
  toMLModelStructureProgram = unsafeCastId

instance IsNSObject (Id MLModelStructureProgram) where
  toNSObject = unsafeCastId

-- ---------- MLModelStructureProgramArgument ----------

-- | A class representing an argument in the Program.
-- 
-- Phantom type for @MLModelStructureProgramArgument@.
data MLModelStructureProgramArgument

instance IsObjCObject (Id MLModelStructureProgramArgument) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLModelStructureProgramArgument"

class IsNSObject a => IsMLModelStructureProgramArgument a where
  toMLModelStructureProgramArgument :: a -> Id MLModelStructureProgramArgument

instance IsMLModelStructureProgramArgument (Id MLModelStructureProgramArgument) where
  toMLModelStructureProgramArgument = unsafeCastId

instance IsNSObject (Id MLModelStructureProgramArgument) where
  toNSObject = unsafeCastId

-- ---------- MLModelStructureProgramBinding ----------

-- | A class representing a binding in the Program
--
-- A Binding is either a previously defined name of a variable or a constant value in the Program.
-- 
-- Phantom type for @MLModelStructureProgramBinding@.
data MLModelStructureProgramBinding

instance IsObjCObject (Id MLModelStructureProgramBinding) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLModelStructureProgramBinding"

class IsNSObject a => IsMLModelStructureProgramBinding a where
  toMLModelStructureProgramBinding :: a -> Id MLModelStructureProgramBinding

instance IsMLModelStructureProgramBinding (Id MLModelStructureProgramBinding) where
  toMLModelStructureProgramBinding = unsafeCastId

instance IsNSObject (Id MLModelStructureProgramBinding) where
  toNSObject = unsafeCastId

-- ---------- MLModelStructureProgramBlock ----------

-- | A class representing a block in the Program.
-- 
-- Phantom type for @MLModelStructureProgramBlock@.
data MLModelStructureProgramBlock

instance IsObjCObject (Id MLModelStructureProgramBlock) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLModelStructureProgramBlock"

class IsNSObject a => IsMLModelStructureProgramBlock a where
  toMLModelStructureProgramBlock :: a -> Id MLModelStructureProgramBlock

instance IsMLModelStructureProgramBlock (Id MLModelStructureProgramBlock) where
  toMLModelStructureProgramBlock = unsafeCastId

instance IsNSObject (Id MLModelStructureProgramBlock) where
  toNSObject = unsafeCastId

-- ---------- MLModelStructureProgramFunction ----------

-- | A class representing a function in the Program.
-- 
-- Phantom type for @MLModelStructureProgramFunction@.
data MLModelStructureProgramFunction

instance IsObjCObject (Id MLModelStructureProgramFunction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLModelStructureProgramFunction"

class IsNSObject a => IsMLModelStructureProgramFunction a where
  toMLModelStructureProgramFunction :: a -> Id MLModelStructureProgramFunction

instance IsMLModelStructureProgramFunction (Id MLModelStructureProgramFunction) where
  toMLModelStructureProgramFunction = unsafeCastId

instance IsNSObject (Id MLModelStructureProgramFunction) where
  toNSObject = unsafeCastId

-- ---------- MLModelStructureProgramNamedValueType ----------

-- | A class representing a named value type in a Program.
-- 
-- Phantom type for @MLModelStructureProgramNamedValueType@.
data MLModelStructureProgramNamedValueType

instance IsObjCObject (Id MLModelStructureProgramNamedValueType) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLModelStructureProgramNamedValueType"

class IsNSObject a => IsMLModelStructureProgramNamedValueType a where
  toMLModelStructureProgramNamedValueType :: a -> Id MLModelStructureProgramNamedValueType

instance IsMLModelStructureProgramNamedValueType (Id MLModelStructureProgramNamedValueType) where
  toMLModelStructureProgramNamedValueType = unsafeCastId

instance IsNSObject (Id MLModelStructureProgramNamedValueType) where
  toNSObject = unsafeCastId

-- ---------- MLModelStructureProgramOperation ----------

-- | A class representing an Operation in a Program.
-- 
-- Phantom type for @MLModelStructureProgramOperation@.
data MLModelStructureProgramOperation

instance IsObjCObject (Id MLModelStructureProgramOperation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLModelStructureProgramOperation"

class IsNSObject a => IsMLModelStructureProgramOperation a where
  toMLModelStructureProgramOperation :: a -> Id MLModelStructureProgramOperation

instance IsMLModelStructureProgramOperation (Id MLModelStructureProgramOperation) where
  toMLModelStructureProgramOperation = unsafeCastId

instance IsNSObject (Id MLModelStructureProgramOperation) where
  toNSObject = unsafeCastId

-- ---------- MLModelStructureProgramValue ----------

-- | A class representing a constant value in the Program.
-- 
-- Phantom type for @MLModelStructureProgramValue@.
data MLModelStructureProgramValue

instance IsObjCObject (Id MLModelStructureProgramValue) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLModelStructureProgramValue"

class IsNSObject a => IsMLModelStructureProgramValue a where
  toMLModelStructureProgramValue :: a -> Id MLModelStructureProgramValue

instance IsMLModelStructureProgramValue (Id MLModelStructureProgramValue) where
  toMLModelStructureProgramValue = unsafeCastId

instance IsNSObject (Id MLModelStructureProgramValue) where
  toNSObject = unsafeCastId

-- ---------- MLModelStructureProgramValueType ----------

-- | A class representing the type of a value or a variable in the Program.
-- 
-- Phantom type for @MLModelStructureProgramValueType@.
data MLModelStructureProgramValueType

instance IsObjCObject (Id MLModelStructureProgramValueType) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLModelStructureProgramValueType"

class IsNSObject a => IsMLModelStructureProgramValueType a where
  toMLModelStructureProgramValueType :: a -> Id MLModelStructureProgramValueType

instance IsMLModelStructureProgramValueType (Id MLModelStructureProgramValueType) where
  toMLModelStructureProgramValueType = unsafeCastId

instance IsNSObject (Id MLModelStructureProgramValueType) where
  toNSObject = unsafeCastId

-- ---------- MLMultiArray ----------

-- | Use @MLMultiArray@ to store a multi-dimensional value.
--
-- Unlike @MLShapedArray@ or @MLTensor@, @MLMultiArray@ can be used in Obj-C code. Unlike @MLTensor@, @MLMultiArray@ is always backed by a concrete storage.
--
-- The object has properties to define the interpretation of the storage.
--
-- @.dataType@ defines the interpretation of raw bytes into a numeric scalar value. For example, @MLMultiArrayDataTypeFloat32@ means the backing storage uses IEEE 754 Float32 encoding.
--
-- @.shape@ defines the multi-dimensional space. For example, 30 x 20 image with three color components (Red, Green, Blue) could be defined using the shape @[3, 20, 30]@.
--
-- @.strides@ defines the offset addressing of the scalar for a given coordinates. For example, the image above might use @[640, 32, 1]@ as the @strides@. Then, the scalar at (1, 10, 15) is stored at @640 * 1 + 32 * 10 + 1 * 15@, or 975th scalar in the storage. In general, the scalar offset for coordinates @index@ and strides @strides@ is:
--
-- ``` scalarOffset = sum_d index[d]*strides[d] ```
--
-- The backing storage can be a heap allocated buffer or CVPixelBuffer. Though CVPixelBuffer backing supports limited data types, @MLModel@ could share the storage with backend hardware such as Apple Neural Engine without copy.
-- 
-- Phantom type for @MLMultiArray@.
data MLMultiArray

instance IsObjCObject (Id MLMultiArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLMultiArray"

class IsNSObject a => IsMLMultiArray a where
  toMLMultiArray :: a -> Id MLMultiArray

instance IsMLMultiArray (Id MLMultiArray) where
  toMLMultiArray = unsafeCastId

instance IsNSObject (Id MLMultiArray) where
  toNSObject = unsafeCastId

-- ---------- MLMultiArrayConstraint ----------

-- | Constraint describing expected MLMultiArray properties
-- 
-- Phantom type for @MLMultiArrayConstraint@.
data MLMultiArrayConstraint

instance IsObjCObject (Id MLMultiArrayConstraint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLMultiArrayConstraint"

class IsNSObject a => IsMLMultiArrayConstraint a where
  toMLMultiArrayConstraint :: a -> Id MLMultiArrayConstraint

instance IsMLMultiArrayConstraint (Id MLMultiArrayConstraint) where
  toMLMultiArrayConstraint = unsafeCastId

instance IsNSObject (Id MLMultiArrayConstraint) where
  toNSObject = unsafeCastId

-- ---------- MLMultiArrayShapeConstraint ----------

-- | Phantom type for @MLMultiArrayShapeConstraint@.
data MLMultiArrayShapeConstraint

instance IsObjCObject (Id MLMultiArrayShapeConstraint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLMultiArrayShapeConstraint"

class IsNSObject a => IsMLMultiArrayShapeConstraint a where
  toMLMultiArrayShapeConstraint :: a -> Id MLMultiArrayShapeConstraint

instance IsMLMultiArrayShapeConstraint (Id MLMultiArrayShapeConstraint) where
  toMLMultiArrayShapeConstraint = unsafeCastId

instance IsNSObject (Id MLMultiArrayShapeConstraint) where
  toNSObject = unsafeCastId

-- ---------- MLNeuralEngineComputeDevice ----------

-- | Represents a NeuralEngine compute device for inference of machine learning models.
-- 
-- Phantom type for @MLNeuralEngineComputeDevice@.
data MLNeuralEngineComputeDevice

instance IsObjCObject (Id MLNeuralEngineComputeDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLNeuralEngineComputeDevice"

class IsNSObject a => IsMLNeuralEngineComputeDevice a where
  toMLNeuralEngineComputeDevice :: a -> Id MLNeuralEngineComputeDevice

instance IsMLNeuralEngineComputeDevice (Id MLNeuralEngineComputeDevice) where
  toMLNeuralEngineComputeDevice = unsafeCastId

instance IsNSObject (Id MLNeuralEngineComputeDevice) where
  toNSObject = unsafeCastId

-- ---------- MLNumericConstraint ----------

-- | Allows enforcement of constraints on the values of update parameters.
-- 
-- Phantom type for @MLNumericConstraint@.
data MLNumericConstraint

instance IsObjCObject (Id MLNumericConstraint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLNumericConstraint"

class IsNSObject a => IsMLNumericConstraint a where
  toMLNumericConstraint :: a -> Id MLNumericConstraint

instance IsMLNumericConstraint (Id MLNumericConstraint) where
  toMLNumericConstraint = unsafeCastId

instance IsNSObject (Id MLNumericConstraint) where
  toNSObject = unsafeCastId

-- ---------- MLOptimizationHints ----------

-- | MLOptimizationHints
--
-- An object to hold hints that CoreML could use for further optimization
-- 
-- Phantom type for @MLOptimizationHints@.
data MLOptimizationHints

instance IsObjCObject (Id MLOptimizationHints) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLOptimizationHints"

class IsNSObject a => IsMLOptimizationHints a where
  toMLOptimizationHints :: a -> Id MLOptimizationHints

instance IsMLOptimizationHints (Id MLOptimizationHints) where
  toMLOptimizationHints = unsafeCastId

instance IsNSObject (Id MLOptimizationHints) where
  toNSObject = unsafeCastId

-- ---------- MLParameterDescription ----------

-- | Describes a model parameter along with a default value and any applicable constaint on the values.
-- 
-- Phantom type for @MLParameterDescription@.
data MLParameterDescription

instance IsObjCObject (Id MLParameterDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLParameterDescription"

class IsNSObject a => IsMLParameterDescription a where
  toMLParameterDescription :: a -> Id MLParameterDescription

instance IsMLParameterDescription (Id MLParameterDescription) where
  toMLParameterDescription = unsafeCastId

instance IsNSObject (Id MLParameterDescription) where
  toNSObject = unsafeCastId

-- ---------- MLPredictionOptions ----------

-- | MLPredictionOptions
--
-- An object to hold options / controls / parameters of how model prediction is performed
-- 
-- Phantom type for @MLPredictionOptions@.
data MLPredictionOptions

instance IsObjCObject (Id MLPredictionOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLPredictionOptions"

class IsNSObject a => IsMLPredictionOptions a where
  toMLPredictionOptions :: a -> Id MLPredictionOptions

instance IsMLPredictionOptions (Id MLPredictionOptions) where
  toMLPredictionOptions = unsafeCastId

instance IsNSObject (Id MLPredictionOptions) where
  toNSObject = unsafeCastId

-- ---------- MLSequence ----------

-- | An immutable container holding an ordered collection of feature values of the same type.
-- 
-- Phantom type for @MLSequence@.
data MLSequence

instance IsObjCObject (Id MLSequence) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLSequence"

class IsNSObject a => IsMLSequence a where
  toMLSequence :: a -> Id MLSequence

instance IsMLSequence (Id MLSequence) where
  toMLSequence = unsafeCastId

instance IsNSObject (Id MLSequence) where
  toNSObject = unsafeCastId

-- ---------- MLSequenceConstraint ----------

-- | Constraint describing expected MLSequence properties
-- 
-- Phantom type for @MLSequenceConstraint@.
data MLSequenceConstraint

instance IsObjCObject (Id MLSequenceConstraint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLSequenceConstraint"

class IsNSObject a => IsMLSequenceConstraint a where
  toMLSequenceConstraint :: a -> Id MLSequenceConstraint

instance IsMLSequenceConstraint (Id MLSequenceConstraint) where
  toMLSequenceConstraint = unsafeCastId

instance IsNSObject (Id MLSequenceConstraint) where
  toNSObject = unsafeCastId

-- ---------- MLState ----------

-- | Handle to the state buffers.
--
-- A stateful model maintains a state from one prediction to another by storing the information in the state buffers. To use such a model, the client must request the model to create state buffers and get @MLState@ object, which is the handle to those buffers. Then, at the prediction time, pass the @MLState@ object in one of the stateful prediction functions.
--
-- ```swift // Load a stateful model let modelAsset = try MLModelAsset(url: modelURL) let model = try await MLModel.load(asset: modelAsset, configuration: MLModelConfiguration())
--
-- // Request a state let state = model.newState()
--
-- // Run predictions for _ in 0 ..< 42 {   _ = try await model.prediction(from: inputFeatures, using: state) }
--
-- // Access the state buffer. state.withMultiArray(for: "accumulator") { stateMultiArray in   ... } ```
--
-- The object is a handle to the state buffers. The client shall not read or write the buffers while a prediction is in-flight.
--
-- Each stateful prediction that uses the same @MLState@ must be serialized. Otherwise, if two such predictions run concurrently, the behavior is undefined.
-- 
-- Phantom type for @MLState@.
data MLState

instance IsObjCObject (Id MLState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLState"

class IsNSObject a => IsMLState a where
  toMLState :: a -> Id MLState

instance IsMLState (Id MLState) where
  toMLState = unsafeCastId

instance IsNSObject (Id MLState) where
  toNSObject = unsafeCastId

-- ---------- MLStateConstraint ----------

-- | Constraint of a state feature value.
-- 
-- Phantom type for @MLStateConstraint@.
data MLStateConstraint

instance IsObjCObject (Id MLStateConstraint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLStateConstraint"

class IsNSObject a => IsMLStateConstraint a where
  toMLStateConstraint :: a -> Id MLStateConstraint

instance IsMLStateConstraint (Id MLStateConstraint) where
  toMLStateConstraint = unsafeCastId

instance IsNSObject (Id MLStateConstraint) where
  toNSObject = unsafeCastId

-- ---------- MLTask ----------

-- | Class that abstracts state transitions and basic task controls.
-- 
-- Phantom type for @MLTask@.
data MLTask

instance IsObjCObject (Id MLTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLTask"

class IsNSObject a => IsMLTask a where
  toMLTask :: a -> Id MLTask

instance IsMLTask (Id MLTask) where
  toMLTask = unsafeCastId

instance IsNSObject (Id MLTask) where
  toNSObject = unsafeCastId

-- ---------- MLUpdateContext ----------

-- | Provides context for the update process when the progress or completion handlers are invoked.
-- 
-- Phantom type for @MLUpdateContext@.
data MLUpdateContext

instance IsObjCObject (Id MLUpdateContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLUpdateContext"

class IsNSObject a => IsMLUpdateContext a where
  toMLUpdateContext :: a -> Id MLUpdateContext

instance IsMLUpdateContext (Id MLUpdateContext) where
  toMLUpdateContext = unsafeCastId

instance IsNSObject (Id MLUpdateContext) where
  toNSObject = unsafeCastId

-- ---------- MLUpdateProgressHandlers ----------

-- | Allows applications to register for progress and completion handlers.
-- 
-- Phantom type for @MLUpdateProgressHandlers@.
data MLUpdateProgressHandlers

instance IsObjCObject (Id MLUpdateProgressHandlers) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLUpdateProgressHandlers"

class IsNSObject a => IsMLUpdateProgressHandlers a where
  toMLUpdateProgressHandlers :: a -> Id MLUpdateProgressHandlers

instance IsMLUpdateProgressHandlers (Id MLUpdateProgressHandlers) where
  toMLUpdateProgressHandlers = unsafeCastId

instance IsNSObject (Id MLUpdateProgressHandlers) where
  toNSObject = unsafeCastId

-- ---------- MLMetricKey ----------

-- | A class to specify list of supported model update metrics.
-- 
-- Phantom type for @MLMetricKey@.
data MLMetricKey

instance IsObjCObject (Id MLMetricKey) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLMetricKey"

class IsMLKey a => IsMLMetricKey a where
  toMLMetricKey :: a -> Id MLMetricKey

instance IsMLMetricKey (Id MLMetricKey) where
  toMLMetricKey = unsafeCastId

instance IsMLKey (Id MLMetricKey) where
  toMLKey = unsafeCastId

instance IsNSObject (Id MLMetricKey) where
  toNSObject = unsafeCastId

-- ---------- MLParameterKey ----------

-- | A class to specify list of supported model update parameters.
-- 
-- Phantom type for @MLParameterKey@.
data MLParameterKey

instance IsObjCObject (Id MLParameterKey) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLParameterKey"

class IsMLKey a => IsMLParameterKey a where
  toMLParameterKey :: a -> Id MLParameterKey

instance IsMLParameterKey (Id MLParameterKey) where
  toMLParameterKey = unsafeCastId

instance IsMLKey (Id MLParameterKey) where
  toMLKey = unsafeCastId

instance IsNSObject (Id MLParameterKey) where
  toNSObject = unsafeCastId

-- ---------- MLUpdateTask ----------

-- | Main class for setting up and controlling a model update. It provides some utility class methods for performing an update synchronously as well as class constructors for configuring an update and give developers control for the execution of that update.
-- 
-- Phantom type for @MLUpdateTask@.
data MLUpdateTask

instance IsObjCObject (Id MLUpdateTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MLUpdateTask"

class IsMLTask a => IsMLUpdateTask a where
  toMLUpdateTask :: a -> Id MLUpdateTask

instance IsMLUpdateTask (Id MLUpdateTask) where
  toMLUpdateTask = unsafeCastId

instance IsMLTask (Id MLUpdateTask) where
  toMLTask = unsafeCastId

instance IsNSObject (Id MLUpdateTask) where
  toNSObject = unsafeCastId
