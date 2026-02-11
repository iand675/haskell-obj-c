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
  , initSelector
  , newSelector
  , loadContentsOfURL_configuration_completionHandlerSelector
  , loadModelAsset_configuration_completionHandlerSelector
  , estimatedCostOfMLProgramOperationSelector
  , computeDeviceUsageForNeuralNetworkLayerSelector
  , computeDeviceUsageForMLProgramOperationSelector
  , modelStructureSelector


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

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMLComputePlan mlComputePlan => mlComputePlan -> IO (Id MLComputePlan)
init_ mlComputePlan  =
  sendMsg mlComputePlan (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MLComputePlan)
new  =
  do
    cls' <- getRequiredClass "MLComputePlan"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    withObjCPtr url $ \raw_url ->
      withObjCPtr configuration $ \raw_configuration ->
        sendClassMsg cls' (mkSelector "loadContentsOfURL:configuration:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

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
    withObjCPtr asset $ \raw_asset ->
      withObjCPtr configuration $ \raw_configuration ->
        sendClassMsg cls' (mkSelector "loadModelAsset:configuration:completionHandler:") retVoid [argPtr (castPtr raw_asset :: Ptr ()), argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | Returns the estimated cost of executing an ML Program operation.
--
-- @operation@ — An ML Program operation.
--
-- The estimated cost of executing the operation or nil if the cost couldn't be estimated.
--
-- ObjC selector: @- estimatedCostOfMLProgramOperation:@
estimatedCostOfMLProgramOperation :: (IsMLComputePlan mlComputePlan, IsMLModelStructureProgramOperation operation) => mlComputePlan -> operation -> IO (Id MLComputePlanCost)
estimatedCostOfMLProgramOperation mlComputePlan  operation =
withObjCPtr operation $ \raw_operation ->
    sendMsg mlComputePlan (mkSelector "estimatedCostOfMLProgramOperation:") (retPtr retVoid) [argPtr (castPtr raw_operation :: Ptr ())] >>= retainedObject . castPtr

-- | Returns the anticipated compute devices that would be used for executing a NeuralNetwork layer.
--
-- @layer@ — A NeuralNetwork layer.
--
-- The anticipated compute devices that would be used for executing the layer or @nil@ if the usage couldn't be determined.
--
-- ObjC selector: @- computeDeviceUsageForNeuralNetworkLayer:@
computeDeviceUsageForNeuralNetworkLayer :: (IsMLComputePlan mlComputePlan, IsMLModelStructureNeuralNetworkLayer layer) => mlComputePlan -> layer -> IO (Id MLComputePlanDeviceUsage)
computeDeviceUsageForNeuralNetworkLayer mlComputePlan  layer =
withObjCPtr layer $ \raw_layer ->
    sendMsg mlComputePlan (mkSelector "computeDeviceUsageForNeuralNetworkLayer:") (retPtr retVoid) [argPtr (castPtr raw_layer :: Ptr ())] >>= retainedObject . castPtr

-- | Returns The anticipated compute devices that would be used for executing an ML Program operation.
--
-- @operation@ — An ML Program operation.
--
-- The anticipated compute devices that would be used for executing the operation or @nil@if the usage couldn't be determined.
--
-- ObjC selector: @- computeDeviceUsageForMLProgramOperation:@
computeDeviceUsageForMLProgramOperation :: (IsMLComputePlan mlComputePlan, IsMLModelStructureProgramOperation operation) => mlComputePlan -> operation -> IO (Id MLComputePlanDeviceUsage)
computeDeviceUsageForMLProgramOperation mlComputePlan  operation =
withObjCPtr operation $ \raw_operation ->
    sendMsg mlComputePlan (mkSelector "computeDeviceUsageForMLProgramOperation:") (retPtr retVoid) [argPtr (castPtr raw_operation :: Ptr ())] >>= retainedObject . castPtr

-- | The model structure.
--
-- ObjC selector: @- modelStructure@
modelStructure :: IsMLComputePlan mlComputePlan => mlComputePlan -> IO (Id MLModelStructure)
modelStructure mlComputePlan  =
  sendMsg mlComputePlan (mkSelector "modelStructure") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @loadContentsOfURL:configuration:completionHandler:@
loadContentsOfURL_configuration_completionHandlerSelector :: Selector
loadContentsOfURL_configuration_completionHandlerSelector = mkSelector "loadContentsOfURL:configuration:completionHandler:"

-- | @Selector@ for @loadModelAsset:configuration:completionHandler:@
loadModelAsset_configuration_completionHandlerSelector :: Selector
loadModelAsset_configuration_completionHandlerSelector = mkSelector "loadModelAsset:configuration:completionHandler:"

-- | @Selector@ for @estimatedCostOfMLProgramOperation:@
estimatedCostOfMLProgramOperationSelector :: Selector
estimatedCostOfMLProgramOperationSelector = mkSelector "estimatedCostOfMLProgramOperation:"

-- | @Selector@ for @computeDeviceUsageForNeuralNetworkLayer:@
computeDeviceUsageForNeuralNetworkLayerSelector :: Selector
computeDeviceUsageForNeuralNetworkLayerSelector = mkSelector "computeDeviceUsageForNeuralNetworkLayer:"

-- | @Selector@ for @computeDeviceUsageForMLProgramOperation:@
computeDeviceUsageForMLProgramOperationSelector :: Selector
computeDeviceUsageForMLProgramOperationSelector = mkSelector "computeDeviceUsageForMLProgramOperation:"

-- | @Selector@ for @modelStructure@
modelStructureSelector :: Selector
modelStructureSelector = mkSelector "modelStructure"

