{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Water Heater Management    This cluster is used to allow clients to control the operation of a hot water heating appliance so that it can be used with energy management.
--
-- Generated bindings for @MTRClusterWaterHeaterManagement@.
module ObjC.Matter.MTRClusterWaterHeaterManagement
  ( MTRClusterWaterHeaterManagement
  , IsMTRClusterWaterHeaterManagement(..)
  , boostWithParams_expectedValues_expectedValueInterval_completion
  , cancelBoostWithParams_expectedValues_expectedValueInterval_completion
  , cancelBoostWithExpectedValues_expectedValueInterval_completion
  , readAttributeHeaterTypesWithParams
  , readAttributeHeatDemandWithParams
  , readAttributeTankVolumeWithParams
  , readAttributeEstimatedHeatRequiredWithParams
  , readAttributeTankPercentageWithParams
  , readAttributeBoostStateWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , boostWithParams_expectedValues_expectedValueInterval_completionSelector
  , cancelBoostWithExpectedValues_expectedValueInterval_completionSelector
  , cancelBoostWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeBoostStateWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeEstimatedHeatRequiredWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeHeatDemandWithParamsSelector
  , readAttributeHeaterTypesWithParamsSelector
  , readAttributeTankPercentageWithParamsSelector
  , readAttributeTankVolumeWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- boostWithParams:expectedValues:expectedValueInterval:completion:@
boostWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRWaterHeaterManagementClusterBoostParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWaterHeaterManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
boostWithParams_expectedValues_expectedValueInterval_completion mtrClusterWaterHeaterManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterWaterHeaterManagement boostWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRWaterHeaterManagementClusterBoostParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- cancelBoostWithParams:expectedValues:expectedValueInterval:completion:@
cancelBoostWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRWaterHeaterManagementClusterCancelBoostParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterWaterHeaterManagement -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
cancelBoostWithParams_expectedValues_expectedValueInterval_completion mtrClusterWaterHeaterManagement params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterWaterHeaterManagement cancelBoostWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRWaterHeaterManagementClusterCancelBoostParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- cancelBoostWithExpectedValues:expectedValueInterval:completion:@
cancelBoostWithExpectedValues_expectedValueInterval_completion :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsNSArray expectedValues, IsNSNumber expectedValueIntervalMs) => mtrClusterWaterHeaterManagement -> expectedValues -> expectedValueIntervalMs -> Ptr () -> IO ()
cancelBoostWithExpectedValues_expectedValueInterval_completion mtrClusterWaterHeaterManagement expectedValues expectedValueIntervalMs completion =
  sendMessage mtrClusterWaterHeaterManagement cancelBoostWithExpectedValues_expectedValueInterval_completionSelector (toNSArray expectedValues) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeHeaterTypesWithParams:@
readAttributeHeaterTypesWithParams :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRReadParams params) => mtrClusterWaterHeaterManagement -> params -> IO (Id NSDictionary)
readAttributeHeaterTypesWithParams mtrClusterWaterHeaterManagement params =
  sendMessage mtrClusterWaterHeaterManagement readAttributeHeaterTypesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeHeatDemandWithParams:@
readAttributeHeatDemandWithParams :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRReadParams params) => mtrClusterWaterHeaterManagement -> params -> IO (Id NSDictionary)
readAttributeHeatDemandWithParams mtrClusterWaterHeaterManagement params =
  sendMessage mtrClusterWaterHeaterManagement readAttributeHeatDemandWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTankVolumeWithParams:@
readAttributeTankVolumeWithParams :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRReadParams params) => mtrClusterWaterHeaterManagement -> params -> IO (Id NSDictionary)
readAttributeTankVolumeWithParams mtrClusterWaterHeaterManagement params =
  sendMessage mtrClusterWaterHeaterManagement readAttributeTankVolumeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeEstimatedHeatRequiredWithParams:@
readAttributeEstimatedHeatRequiredWithParams :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRReadParams params) => mtrClusterWaterHeaterManagement -> params -> IO (Id NSDictionary)
readAttributeEstimatedHeatRequiredWithParams mtrClusterWaterHeaterManagement params =
  sendMessage mtrClusterWaterHeaterManagement readAttributeEstimatedHeatRequiredWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTankPercentageWithParams:@
readAttributeTankPercentageWithParams :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRReadParams params) => mtrClusterWaterHeaterManagement -> params -> IO (Id NSDictionary)
readAttributeTankPercentageWithParams mtrClusterWaterHeaterManagement params =
  sendMessage mtrClusterWaterHeaterManagement readAttributeTankPercentageWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeBoostStateWithParams:@
readAttributeBoostStateWithParams :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRReadParams params) => mtrClusterWaterHeaterManagement -> params -> IO (Id NSDictionary)
readAttributeBoostStateWithParams mtrClusterWaterHeaterManagement params =
  sendMessage mtrClusterWaterHeaterManagement readAttributeBoostStateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRReadParams params) => mtrClusterWaterHeaterManagement -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterWaterHeaterManagement params =
  sendMessage mtrClusterWaterHeaterManagement readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRReadParams params) => mtrClusterWaterHeaterManagement -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterWaterHeaterManagement params =
  sendMessage mtrClusterWaterHeaterManagement readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRReadParams params) => mtrClusterWaterHeaterManagement -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterWaterHeaterManagement params =
  sendMessage mtrClusterWaterHeaterManagement readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRReadParams params) => mtrClusterWaterHeaterManagement -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterWaterHeaterManagement params =
  sendMessage mtrClusterWaterHeaterManagement readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRReadParams params) => mtrClusterWaterHeaterManagement -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterWaterHeaterManagement params =
  sendMessage mtrClusterWaterHeaterManagement readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement => mtrClusterWaterHeaterManagement -> IO (Id MTRClusterWaterHeaterManagement)
init_ mtrClusterWaterHeaterManagement =
  sendOwnedMessage mtrClusterWaterHeaterManagement initSelector

-- | @+ new@
new :: IO (Id MTRClusterWaterHeaterManagement)
new  =
  do
    cls' <- getRequiredClass "MTRClusterWaterHeaterManagement"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterWaterHeaterManagement mtrClusterWaterHeaterManagement, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterWaterHeaterManagement -> device -> endpointID -> queue -> IO (Id MTRClusterWaterHeaterManagement)
initWithDevice_endpointID_queue mtrClusterWaterHeaterManagement device endpointID queue =
  sendOwnedMessage mtrClusterWaterHeaterManagement initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @boostWithParams:expectedValues:expectedValueInterval:completion:@
boostWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRWaterHeaterManagementClusterBoostParams, Id NSArray, Id NSNumber, Ptr ()] ()
boostWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "boostWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @cancelBoostWithParams:expectedValues:expectedValueInterval:completion:@
cancelBoostWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRWaterHeaterManagementClusterCancelBoostParams, Id NSArray, Id NSNumber, Ptr ()] ()
cancelBoostWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "cancelBoostWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @cancelBoostWithExpectedValues:expectedValueInterval:completion:@
cancelBoostWithExpectedValues_expectedValueInterval_completionSelector :: Selector '[Id NSArray, Id NSNumber, Ptr ()] ()
cancelBoostWithExpectedValues_expectedValueInterval_completionSelector = mkSelector "cancelBoostWithExpectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeHeaterTypesWithParams:@
readAttributeHeaterTypesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeHeaterTypesWithParamsSelector = mkSelector "readAttributeHeaterTypesWithParams:"

-- | @Selector@ for @readAttributeHeatDemandWithParams:@
readAttributeHeatDemandWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeHeatDemandWithParamsSelector = mkSelector "readAttributeHeatDemandWithParams:"

-- | @Selector@ for @readAttributeTankVolumeWithParams:@
readAttributeTankVolumeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTankVolumeWithParamsSelector = mkSelector "readAttributeTankVolumeWithParams:"

-- | @Selector@ for @readAttributeEstimatedHeatRequiredWithParams:@
readAttributeEstimatedHeatRequiredWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeEstimatedHeatRequiredWithParamsSelector = mkSelector "readAttributeEstimatedHeatRequiredWithParams:"

-- | @Selector@ for @readAttributeTankPercentageWithParams:@
readAttributeTankPercentageWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTankPercentageWithParamsSelector = mkSelector "readAttributeTankPercentageWithParams:"

-- | @Selector@ for @readAttributeBoostStateWithParams:@
readAttributeBoostStateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeBoostStateWithParamsSelector = mkSelector "readAttributeBoostStateWithParams:"

-- | @Selector@ for @readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeGeneratedCommandListWithParamsSelector = mkSelector "readAttributeGeneratedCommandListWithParams:"

-- | @Selector@ for @readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAcceptedCommandListWithParamsSelector = mkSelector "readAttributeAcceptedCommandListWithParams:"

-- | @Selector@ for @readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeAttributeListWithParamsSelector = mkSelector "readAttributeAttributeListWithParams:"

-- | @Selector@ for @readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeFeatureMapWithParamsSelector = mkSelector "readAttributeFeatureMapWithParams:"

-- | @Selector@ for @readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeClusterRevisionWithParamsSelector = mkSelector "readAttributeClusterRevisionWithParams:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRClusterWaterHeaterManagement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterWaterHeaterManagement)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterWaterHeaterManagement)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

