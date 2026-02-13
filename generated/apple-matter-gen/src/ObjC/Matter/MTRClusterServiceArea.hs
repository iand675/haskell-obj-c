{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Service Area    The Service Area cluster provides an interface for controlling the areas where a device should operate, and for querying the current area being serviced.
--
-- Generated bindings for @MTRClusterServiceArea@.
module ObjC.Matter.MTRClusterServiceArea
  ( MTRClusterServiceArea
  , IsMTRClusterServiceArea(..)
  , selectAreasWithParams_expectedValues_expectedValueInterval_completion
  , skipAreaWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeSupportedAreasWithParams
  , readAttributeSupportedMapsWithParams
  , readAttributeSelectedAreasWithParams
  , readAttributeCurrentAreaWithParams
  , readAttributeEstimatedEndTimeWithParams
  , readAttributeProgressWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCurrentAreaWithParamsSelector
  , readAttributeEstimatedEndTimeWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeProgressWithParamsSelector
  , readAttributeSelectedAreasWithParamsSelector
  , readAttributeSupportedAreasWithParamsSelector
  , readAttributeSupportedMapsWithParamsSelector
  , selectAreasWithParams_expectedValues_expectedValueInterval_completionSelector
  , skipAreaWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- selectAreasWithParams:expectedValues:expectedValueInterval:completion:@
selectAreasWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRServiceAreaClusterSelectAreasParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterServiceArea -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
selectAreasWithParams_expectedValues_expectedValueInterval_completion mtrClusterServiceArea params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterServiceArea selectAreasWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRServiceAreaClusterSelectAreasParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- skipAreaWithParams:expectedValues:expectedValueInterval:completion:@
skipAreaWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRServiceAreaClusterSkipAreaParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterServiceArea -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
skipAreaWithParams_expectedValues_expectedValueInterval_completion mtrClusterServiceArea params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterServiceArea skipAreaWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRServiceAreaClusterSkipAreaParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeSupportedAreasWithParams:@
readAttributeSupportedAreasWithParams :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRReadParams params) => mtrClusterServiceArea -> params -> IO (Id NSDictionary)
readAttributeSupportedAreasWithParams mtrClusterServiceArea params =
  sendMessage mtrClusterServiceArea readAttributeSupportedAreasWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSupportedMapsWithParams:@
readAttributeSupportedMapsWithParams :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRReadParams params) => mtrClusterServiceArea -> params -> IO (Id NSDictionary)
readAttributeSupportedMapsWithParams mtrClusterServiceArea params =
  sendMessage mtrClusterServiceArea readAttributeSupportedMapsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSelectedAreasWithParams:@
readAttributeSelectedAreasWithParams :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRReadParams params) => mtrClusterServiceArea -> params -> IO (Id NSDictionary)
readAttributeSelectedAreasWithParams mtrClusterServiceArea params =
  sendMessage mtrClusterServiceArea readAttributeSelectedAreasWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentAreaWithParams:@
readAttributeCurrentAreaWithParams :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRReadParams params) => mtrClusterServiceArea -> params -> IO (Id NSDictionary)
readAttributeCurrentAreaWithParams mtrClusterServiceArea params =
  sendMessage mtrClusterServiceArea readAttributeCurrentAreaWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeEstimatedEndTimeWithParams:@
readAttributeEstimatedEndTimeWithParams :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRReadParams params) => mtrClusterServiceArea -> params -> IO (Id NSDictionary)
readAttributeEstimatedEndTimeWithParams mtrClusterServiceArea params =
  sendMessage mtrClusterServiceArea readAttributeEstimatedEndTimeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeProgressWithParams:@
readAttributeProgressWithParams :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRReadParams params) => mtrClusterServiceArea -> params -> IO (Id NSDictionary)
readAttributeProgressWithParams mtrClusterServiceArea params =
  sendMessage mtrClusterServiceArea readAttributeProgressWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRReadParams params) => mtrClusterServiceArea -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterServiceArea params =
  sendMessage mtrClusterServiceArea readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRReadParams params) => mtrClusterServiceArea -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterServiceArea params =
  sendMessage mtrClusterServiceArea readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRReadParams params) => mtrClusterServiceArea -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterServiceArea params =
  sendMessage mtrClusterServiceArea readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRReadParams params) => mtrClusterServiceArea -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterServiceArea params =
  sendMessage mtrClusterServiceArea readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRReadParams params) => mtrClusterServiceArea -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterServiceArea params =
  sendMessage mtrClusterServiceArea readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterServiceArea mtrClusterServiceArea => mtrClusterServiceArea -> IO (Id MTRClusterServiceArea)
init_ mtrClusterServiceArea =
  sendOwnedMessage mtrClusterServiceArea initSelector

-- | @+ new@
new :: IO (Id MTRClusterServiceArea)
new  =
  do
    cls' <- getRequiredClass "MTRClusterServiceArea"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterServiceArea mtrClusterServiceArea, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterServiceArea -> device -> endpointID -> queue -> IO (Id MTRClusterServiceArea)
initWithDevice_endpointID_queue mtrClusterServiceArea device endpointID queue =
  sendOwnedMessage mtrClusterServiceArea initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectAreasWithParams:expectedValues:expectedValueInterval:completion:@
selectAreasWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRServiceAreaClusterSelectAreasParams, Id NSArray, Id NSNumber, Ptr ()] ()
selectAreasWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "selectAreasWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @skipAreaWithParams:expectedValues:expectedValueInterval:completion:@
skipAreaWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRServiceAreaClusterSkipAreaParams, Id NSArray, Id NSNumber, Ptr ()] ()
skipAreaWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "skipAreaWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeSupportedAreasWithParams:@
readAttributeSupportedAreasWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportedAreasWithParamsSelector = mkSelector "readAttributeSupportedAreasWithParams:"

-- | @Selector@ for @readAttributeSupportedMapsWithParams:@
readAttributeSupportedMapsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportedMapsWithParamsSelector = mkSelector "readAttributeSupportedMapsWithParams:"

-- | @Selector@ for @readAttributeSelectedAreasWithParams:@
readAttributeSelectedAreasWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSelectedAreasWithParamsSelector = mkSelector "readAttributeSelectedAreasWithParams:"

-- | @Selector@ for @readAttributeCurrentAreaWithParams:@
readAttributeCurrentAreaWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentAreaWithParamsSelector = mkSelector "readAttributeCurrentAreaWithParams:"

-- | @Selector@ for @readAttributeEstimatedEndTimeWithParams:@
readAttributeEstimatedEndTimeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeEstimatedEndTimeWithParamsSelector = mkSelector "readAttributeEstimatedEndTimeWithParams:"

-- | @Selector@ for @readAttributeProgressWithParams:@
readAttributeProgressWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeProgressWithParamsSelector = mkSelector "readAttributeProgressWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterServiceArea)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterServiceArea)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterServiceArea)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

