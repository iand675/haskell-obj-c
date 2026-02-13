{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

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
-- Generated bindings for @MLComputePlan@.
module ObjC.CoreML.MLComputePlan
  ( MLComputePlan
  , IsMLComputePlan(..)
  , init_
  , new
  , loadContentsOfURL_configuration_completionHandler
  , loadModelAsset_configuration_completionHandler
  , estimatedCostOfMLProgramOperation
  , computeDeviceUsageForNeuralNetworkLayer
  , computeDeviceUsageForMLProgramOperation
  , modelStructure
  , computeDeviceUsageForMLProgramOperationSelector
  , computeDeviceUsageForNeuralNetworkLayerSelector
  , estimatedCostOfMLProgramOperationSelector
  , initSelector
  , loadContentsOfURL_configuration_completionHandlerSelector
  , loadModelAsset_configuration_completionHandlerSelector
  , modelStructureSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMLComputePlan mlComputePlan => mlComputePlan -> IO (Id MLComputePlan)
init_ mlComputePlan =
  sendOwnedMessage mlComputePlan initSelector

-- | @+ new@
new :: IO (Id MLComputePlan)
new  =
  do
    cls' <- getRequiredClass "MLComputePlan"
    sendOwnedClassMessage cls' newSelector

-- | Construct the compute plan of a model asynchronously given the location of its on-disk representation.
--
-- @url@ — The location of its on-disk representation (.mlmodelc directory).
--
-- @configuration@ — The model configuration.
--
-- @handler@ — When the compute plan is constructed successfully or unsuccessfully, the completion handler is invoked with a valid MLComputePlan instance or NSError object.
--
-- ObjC selector: @+ loadContentsOfURL:configuration:completionHandler:@
loadContentsOfURL_configuration_completionHandler :: (IsNSURL url, IsMLModelConfiguration configuration) => url -> configuration -> Ptr () -> IO ()
loadContentsOfURL_configuration_completionHandler url configuration handler =
  do
    cls' <- getRequiredClass "MLComputePlan"
    sendClassMessage cls' loadContentsOfURL_configuration_completionHandlerSelector (toNSURL url) (toMLModelConfiguration configuration) handler

-- | Construct the compute plan of a model asynchronously given the model asset.
--
-- @asset@ — The model asset.
--
-- @configuration@ — The model configuration.
--
-- @handler@ — When the compute plan is constructed successfully or unsuccessfully, the completion handler is invoked with a valid MLComputePlan instance or NSError object.
--
-- ObjC selector: @+ loadModelAsset:configuration:completionHandler:@
loadModelAsset_configuration_completionHandler :: (IsMLModelAsset asset, IsMLModelConfiguration configuration) => asset -> configuration -> Ptr () -> IO ()
loadModelAsset_configuration_completionHandler asset configuration handler =
  do
    cls' <- getRequiredClass "MLComputePlan"
    sendClassMessage cls' loadModelAsset_configuration_completionHandlerSelector (toMLModelAsset asset) (toMLModelConfiguration configuration) handler

-- | Returns the estimated cost of executing an ML Program operation.
--
-- @operation@ — An ML Program operation.
--
-- The estimated cost of executing the operation or nil if the cost couldn't be estimated.
--
-- ObjC selector: @- estimatedCostOfMLProgramOperation:@
estimatedCostOfMLProgramOperation :: (IsMLComputePlan mlComputePlan, IsMLModelStructureProgramOperation operation) => mlComputePlan -> operation -> IO (Id MLComputePlanCost)
estimatedCostOfMLProgramOperation mlComputePlan operation =
  sendMessage mlComputePlan estimatedCostOfMLProgramOperationSelector (toMLModelStructureProgramOperation operation)

-- | Returns the anticipated compute devices that would be used for executing a NeuralNetwork layer.
--
-- @layer@ — A NeuralNetwork layer.
--
-- The anticipated compute devices that would be used for executing the layer or @nil@ if the usage couldn't be determined.
--
-- ObjC selector: @- computeDeviceUsageForNeuralNetworkLayer:@
computeDeviceUsageForNeuralNetworkLayer :: (IsMLComputePlan mlComputePlan, IsMLModelStructureNeuralNetworkLayer layer) => mlComputePlan -> layer -> IO (Id MLComputePlanDeviceUsage)
computeDeviceUsageForNeuralNetworkLayer mlComputePlan layer =
  sendMessage mlComputePlan computeDeviceUsageForNeuralNetworkLayerSelector (toMLModelStructureNeuralNetworkLayer layer)

-- | Returns The anticipated compute devices that would be used for executing an ML Program operation.
--
-- @operation@ — An ML Program operation.
--
-- The anticipated compute devices that would be used for executing the operation or @nil@if the usage couldn't be determined.
--
-- ObjC selector: @- computeDeviceUsageForMLProgramOperation:@
computeDeviceUsageForMLProgramOperation :: (IsMLComputePlan mlComputePlan, IsMLModelStructureProgramOperation operation) => mlComputePlan -> operation -> IO (Id MLComputePlanDeviceUsage)
computeDeviceUsageForMLProgramOperation mlComputePlan operation =
  sendMessage mlComputePlan computeDeviceUsageForMLProgramOperationSelector (toMLModelStructureProgramOperation operation)

-- | The model structure.
--
-- ObjC selector: @- modelStructure@
modelStructure :: IsMLComputePlan mlComputePlan => mlComputePlan -> IO (Id MLModelStructure)
modelStructure mlComputePlan =
  sendMessage mlComputePlan modelStructureSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLComputePlan)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLComputePlan)
newSelector = mkSelector "new"

-- | @Selector@ for @loadContentsOfURL:configuration:completionHandler:@
loadContentsOfURL_configuration_completionHandlerSelector :: Selector '[Id NSURL, Id MLModelConfiguration, Ptr ()] ()
loadContentsOfURL_configuration_completionHandlerSelector = mkSelector "loadContentsOfURL:configuration:completionHandler:"

-- | @Selector@ for @loadModelAsset:configuration:completionHandler:@
loadModelAsset_configuration_completionHandlerSelector :: Selector '[Id MLModelAsset, Id MLModelConfiguration, Ptr ()] ()
loadModelAsset_configuration_completionHandlerSelector = mkSelector "loadModelAsset:configuration:completionHandler:"

-- | @Selector@ for @estimatedCostOfMLProgramOperation:@
estimatedCostOfMLProgramOperationSelector :: Selector '[Id MLModelStructureProgramOperation] (Id MLComputePlanCost)
estimatedCostOfMLProgramOperationSelector = mkSelector "estimatedCostOfMLProgramOperation:"

-- | @Selector@ for @computeDeviceUsageForNeuralNetworkLayer:@
computeDeviceUsageForNeuralNetworkLayerSelector :: Selector '[Id MLModelStructureNeuralNetworkLayer] (Id MLComputePlanDeviceUsage)
computeDeviceUsageForNeuralNetworkLayerSelector = mkSelector "computeDeviceUsageForNeuralNetworkLayer:"

-- | @Selector@ for @computeDeviceUsageForMLProgramOperation:@
computeDeviceUsageForMLProgramOperationSelector :: Selector '[Id MLModelStructureProgramOperation] (Id MLComputePlanDeviceUsage)
computeDeviceUsageForMLProgramOperationSelector = mkSelector "computeDeviceUsageForMLProgramOperation:"

-- | @Selector@ for @modelStructure@
modelStructureSelector :: Selector '[] (Id MLModelStructure)
modelStructureSelector = mkSelector "modelStructure"

