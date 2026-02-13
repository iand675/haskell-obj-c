{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Time Synchronization    Accurate time is required for a number of reasons, including scheduling, display and validating security materials.
--
-- Generated bindings for @MTRClusterTimeSynchronization@.
module ObjC.Matter.MTRClusterTimeSynchronization
  ( MTRClusterTimeSynchronization
  , IsMTRClusterTimeSynchronization(..)
  , setUTCTimeWithParams_expectedValues_expectedValueInterval_completion
  , setTrustedTimeSourceWithParams_expectedValues_expectedValueInterval_completion
  , setTimeZoneWithParams_expectedValues_expectedValueInterval_completion
  , setDSTOffsetWithParams_expectedValues_expectedValueInterval_completion
  , setDefaultNTPWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeUTCTimeWithParams
  , readAttributeGranularityWithParams
  , readAttributeTimeSourceWithParams
  , readAttributeTrustedTimeSourceWithParams
  , readAttributeDefaultNTPWithParams
  , readAttributeTimeZoneWithParams
  , readAttributeDSTOffsetWithParams
  , readAttributeLocalTimeWithParams
  , readAttributeTimeZoneDatabaseWithParams
  , readAttributeNTPServerAvailableWithParams
  , readAttributeTimeZoneListMaxSizeWithParams
  , readAttributeDSTOffsetListMaxSizeWithParams
  , readAttributeSupportsDNSResolveWithParams
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
  , readAttributeDSTOffsetListMaxSizeWithParamsSelector
  , readAttributeDSTOffsetWithParamsSelector
  , readAttributeDefaultNTPWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeGranularityWithParamsSelector
  , readAttributeLocalTimeWithParamsSelector
  , readAttributeNTPServerAvailableWithParamsSelector
  , readAttributeSupportsDNSResolveWithParamsSelector
  , readAttributeTimeSourceWithParamsSelector
  , readAttributeTimeZoneDatabaseWithParamsSelector
  , readAttributeTimeZoneListMaxSizeWithParamsSelector
  , readAttributeTimeZoneWithParamsSelector
  , readAttributeTrustedTimeSourceWithParamsSelector
  , readAttributeUTCTimeWithParamsSelector
  , setDSTOffsetWithParams_expectedValues_expectedValueInterval_completionSelector
  , setDefaultNTPWithParams_expectedValues_expectedValueInterval_completionSelector
  , setTimeZoneWithParams_expectedValues_expectedValueInterval_completionSelector
  , setTrustedTimeSourceWithParams_expectedValues_expectedValueInterval_completionSelector
  , setUTCTimeWithParams_expectedValues_expectedValueInterval_completionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- setUTCTimeWithParams:expectedValues:expectedValueInterval:completion:@
setUTCTimeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRTimeSynchronizationClusterSetUTCTimeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTimeSynchronization -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setUTCTimeWithParams_expectedValues_expectedValueInterval_completion mtrClusterTimeSynchronization params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterTimeSynchronization setUTCTimeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRTimeSynchronizationClusterSetUTCTimeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- setTrustedTimeSourceWithParams:expectedValues:expectedValueInterval:completion:@
setTrustedTimeSourceWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRTimeSynchronizationClusterSetTrustedTimeSourceParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTimeSynchronization -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setTrustedTimeSourceWithParams_expectedValues_expectedValueInterval_completion mtrClusterTimeSynchronization params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterTimeSynchronization setTrustedTimeSourceWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRTimeSynchronizationClusterSetTrustedTimeSourceParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- setTimeZoneWithParams:expectedValues:expectedValueInterval:completion:@
setTimeZoneWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRTimeSynchronizationClusterSetTimeZoneParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTimeSynchronization -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setTimeZoneWithParams_expectedValues_expectedValueInterval_completion mtrClusterTimeSynchronization params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterTimeSynchronization setTimeZoneWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRTimeSynchronizationClusterSetTimeZoneParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- setDSTOffsetWithParams:expectedValues:expectedValueInterval:completion:@
setDSTOffsetWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRTimeSynchronizationClusterSetDSTOffsetParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTimeSynchronization -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setDSTOffsetWithParams_expectedValues_expectedValueInterval_completion mtrClusterTimeSynchronization params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterTimeSynchronization setDSTOffsetWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRTimeSynchronizationClusterSetDSTOffsetParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- setDefaultNTPWithParams:expectedValues:expectedValueInterval:completion:@
setDefaultNTPWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRTimeSynchronizationClusterSetDefaultNTPParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterTimeSynchronization -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
setDefaultNTPWithParams_expectedValues_expectedValueInterval_completion mtrClusterTimeSynchronization params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterTimeSynchronization setDefaultNTPWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRTimeSynchronizationClusterSetDefaultNTPParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeUTCTimeWithParams:@
readAttributeUTCTimeWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeUTCTimeWithParams mtrClusterTimeSynchronization params =
  sendMessage mtrClusterTimeSynchronization readAttributeUTCTimeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGranularityWithParams:@
readAttributeGranularityWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeGranularityWithParams mtrClusterTimeSynchronization params =
  sendMessage mtrClusterTimeSynchronization readAttributeGranularityWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTimeSourceWithParams:@
readAttributeTimeSourceWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeTimeSourceWithParams mtrClusterTimeSynchronization params =
  sendMessage mtrClusterTimeSynchronization readAttributeTimeSourceWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTrustedTimeSourceWithParams:@
readAttributeTrustedTimeSourceWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeTrustedTimeSourceWithParams mtrClusterTimeSynchronization params =
  sendMessage mtrClusterTimeSynchronization readAttributeTrustedTimeSourceWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDefaultNTPWithParams:@
readAttributeDefaultNTPWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeDefaultNTPWithParams mtrClusterTimeSynchronization params =
  sendMessage mtrClusterTimeSynchronization readAttributeDefaultNTPWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTimeZoneWithParams:@
readAttributeTimeZoneWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeTimeZoneWithParams mtrClusterTimeSynchronization params =
  sendMessage mtrClusterTimeSynchronization readAttributeTimeZoneWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDSTOffsetWithParams:@
readAttributeDSTOffsetWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeDSTOffsetWithParams mtrClusterTimeSynchronization params =
  sendMessage mtrClusterTimeSynchronization readAttributeDSTOffsetWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLocalTimeWithParams:@
readAttributeLocalTimeWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeLocalTimeWithParams mtrClusterTimeSynchronization params =
  sendMessage mtrClusterTimeSynchronization readAttributeLocalTimeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTimeZoneDatabaseWithParams:@
readAttributeTimeZoneDatabaseWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeTimeZoneDatabaseWithParams mtrClusterTimeSynchronization params =
  sendMessage mtrClusterTimeSynchronization readAttributeTimeZoneDatabaseWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeNTPServerAvailableWithParams:@
readAttributeNTPServerAvailableWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeNTPServerAvailableWithParams mtrClusterTimeSynchronization params =
  sendMessage mtrClusterTimeSynchronization readAttributeNTPServerAvailableWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeTimeZoneListMaxSizeWithParams:@
readAttributeTimeZoneListMaxSizeWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeTimeZoneListMaxSizeWithParams mtrClusterTimeSynchronization params =
  sendMessage mtrClusterTimeSynchronization readAttributeTimeZoneListMaxSizeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeDSTOffsetListMaxSizeWithParams:@
readAttributeDSTOffsetListMaxSizeWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeDSTOffsetListMaxSizeWithParams mtrClusterTimeSynchronization params =
  sendMessage mtrClusterTimeSynchronization readAttributeDSTOffsetListMaxSizeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSupportsDNSResolveWithParams:@
readAttributeSupportsDNSResolveWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeSupportsDNSResolveWithParams mtrClusterTimeSynchronization params =
  sendMessage mtrClusterTimeSynchronization readAttributeSupportsDNSResolveWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterTimeSynchronization params =
  sendMessage mtrClusterTimeSynchronization readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterTimeSynchronization params =
  sendMessage mtrClusterTimeSynchronization readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterTimeSynchronization params =
  sendMessage mtrClusterTimeSynchronization readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterTimeSynchronization params =
  sendMessage mtrClusterTimeSynchronization readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRReadParams params) => mtrClusterTimeSynchronization -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterTimeSynchronization params =
  sendMessage mtrClusterTimeSynchronization readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization => mtrClusterTimeSynchronization -> IO (Id MTRClusterTimeSynchronization)
init_ mtrClusterTimeSynchronization =
  sendOwnedMessage mtrClusterTimeSynchronization initSelector

-- | @+ new@
new :: IO (Id MTRClusterTimeSynchronization)
new  =
  do
    cls' <- getRequiredClass "MTRClusterTimeSynchronization"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterTimeSynchronization mtrClusterTimeSynchronization, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterTimeSynchronization -> device -> endpointID -> queue -> IO (Id MTRClusterTimeSynchronization)
initWithDevice_endpointID_queue mtrClusterTimeSynchronization device endpointID queue =
  sendOwnedMessage mtrClusterTimeSynchronization initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setUTCTimeWithParams:expectedValues:expectedValueInterval:completion:@
setUTCTimeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRTimeSynchronizationClusterSetUTCTimeParams, Id NSArray, Id NSNumber, Ptr ()] ()
setUTCTimeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setUTCTimeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setTrustedTimeSourceWithParams:expectedValues:expectedValueInterval:completion:@
setTrustedTimeSourceWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRTimeSynchronizationClusterSetTrustedTimeSourceParams, Id NSArray, Id NSNumber, Ptr ()] ()
setTrustedTimeSourceWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setTrustedTimeSourceWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setTimeZoneWithParams:expectedValues:expectedValueInterval:completion:@
setTimeZoneWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRTimeSynchronizationClusterSetTimeZoneParams, Id NSArray, Id NSNumber, Ptr ()] ()
setTimeZoneWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setTimeZoneWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setDSTOffsetWithParams:expectedValues:expectedValueInterval:completion:@
setDSTOffsetWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRTimeSynchronizationClusterSetDSTOffsetParams, Id NSArray, Id NSNumber, Ptr ()] ()
setDSTOffsetWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setDSTOffsetWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @setDefaultNTPWithParams:expectedValues:expectedValueInterval:completion:@
setDefaultNTPWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRTimeSynchronizationClusterSetDefaultNTPParams, Id NSArray, Id NSNumber, Ptr ()] ()
setDefaultNTPWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "setDefaultNTPWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeUTCTimeWithParams:@
readAttributeUTCTimeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeUTCTimeWithParamsSelector = mkSelector "readAttributeUTCTimeWithParams:"

-- | @Selector@ for @readAttributeGranularityWithParams:@
readAttributeGranularityWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeGranularityWithParamsSelector = mkSelector "readAttributeGranularityWithParams:"

-- | @Selector@ for @readAttributeTimeSourceWithParams:@
readAttributeTimeSourceWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTimeSourceWithParamsSelector = mkSelector "readAttributeTimeSourceWithParams:"

-- | @Selector@ for @readAttributeTrustedTimeSourceWithParams:@
readAttributeTrustedTimeSourceWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTrustedTimeSourceWithParamsSelector = mkSelector "readAttributeTrustedTimeSourceWithParams:"

-- | @Selector@ for @readAttributeDefaultNTPWithParams:@
readAttributeDefaultNTPWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDefaultNTPWithParamsSelector = mkSelector "readAttributeDefaultNTPWithParams:"

-- | @Selector@ for @readAttributeTimeZoneWithParams:@
readAttributeTimeZoneWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTimeZoneWithParamsSelector = mkSelector "readAttributeTimeZoneWithParams:"

-- | @Selector@ for @readAttributeDSTOffsetWithParams:@
readAttributeDSTOffsetWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDSTOffsetWithParamsSelector = mkSelector "readAttributeDSTOffsetWithParams:"

-- | @Selector@ for @readAttributeLocalTimeWithParams:@
readAttributeLocalTimeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLocalTimeWithParamsSelector = mkSelector "readAttributeLocalTimeWithParams:"

-- | @Selector@ for @readAttributeTimeZoneDatabaseWithParams:@
readAttributeTimeZoneDatabaseWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTimeZoneDatabaseWithParamsSelector = mkSelector "readAttributeTimeZoneDatabaseWithParams:"

-- | @Selector@ for @readAttributeNTPServerAvailableWithParams:@
readAttributeNTPServerAvailableWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNTPServerAvailableWithParamsSelector = mkSelector "readAttributeNTPServerAvailableWithParams:"

-- | @Selector@ for @readAttributeTimeZoneListMaxSizeWithParams:@
readAttributeTimeZoneListMaxSizeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeTimeZoneListMaxSizeWithParamsSelector = mkSelector "readAttributeTimeZoneListMaxSizeWithParams:"

-- | @Selector@ for @readAttributeDSTOffsetListMaxSizeWithParams:@
readAttributeDSTOffsetListMaxSizeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDSTOffsetListMaxSizeWithParamsSelector = mkSelector "readAttributeDSTOffsetListMaxSizeWithParams:"

-- | @Selector@ for @readAttributeSupportsDNSResolveWithParams:@
readAttributeSupportsDNSResolveWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportsDNSResolveWithParamsSelector = mkSelector "readAttributeSupportsDNSResolveWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterTimeSynchronization)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterTimeSynchronization)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterTimeSynchronization)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

