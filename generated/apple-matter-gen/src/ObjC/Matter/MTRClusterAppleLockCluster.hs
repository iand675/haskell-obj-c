{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Apple Lock Cluster    This lock cluster provides Apple-specific information/configuration for the lock.
--
-- Generated bindings for @MTRClusterAppleLockCluster@.
module ObjC.Matter.MTRClusterAppleLockCluster
  ( MTRClusterAppleLockCluster
  , IsMTRClusterAppleLockCluster(..)
  , readAttributeIntentDetectionWithParams
  , writeAttributeIntentDetectionWithValue_expectedValueInterval
  , writeAttributeIntentDetectionWithValue_expectedValueInterval_params
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
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeIntentDetectionWithParamsSelector
  , writeAttributeIntentDetectionWithValue_expectedValueIntervalSelector
  , writeAttributeIntentDetectionWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeIntentDetectionWithParams:@
readAttributeIntentDetectionWithParams :: (IsMTRClusterAppleLockCluster mtrClusterAppleLockCluster, IsMTRReadParams params) => mtrClusterAppleLockCluster -> params -> IO (Id NSDictionary)
readAttributeIntentDetectionWithParams mtrClusterAppleLockCluster params =
  sendMessage mtrClusterAppleLockCluster readAttributeIntentDetectionWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeIntentDetectionWithValue:expectedValueInterval:@
writeAttributeIntentDetectionWithValue_expectedValueInterval :: (IsMTRClusterAppleLockCluster mtrClusterAppleLockCluster, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterAppleLockCluster -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeIntentDetectionWithValue_expectedValueInterval mtrClusterAppleLockCluster dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterAppleLockCluster writeAttributeIntentDetectionWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeIntentDetectionWithValue:expectedValueInterval:params:@
writeAttributeIntentDetectionWithValue_expectedValueInterval_params :: (IsMTRClusterAppleLockCluster mtrClusterAppleLockCluster, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterAppleLockCluster -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeIntentDetectionWithValue_expectedValueInterval_params mtrClusterAppleLockCluster dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterAppleLockCluster writeAttributeIntentDetectionWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterAppleLockCluster mtrClusterAppleLockCluster, IsMTRReadParams params) => mtrClusterAppleLockCluster -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterAppleLockCluster params =
  sendMessage mtrClusterAppleLockCluster readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterAppleLockCluster mtrClusterAppleLockCluster, IsMTRReadParams params) => mtrClusterAppleLockCluster -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterAppleLockCluster params =
  sendMessage mtrClusterAppleLockCluster readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterAppleLockCluster mtrClusterAppleLockCluster, IsMTRReadParams params) => mtrClusterAppleLockCluster -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterAppleLockCluster params =
  sendMessage mtrClusterAppleLockCluster readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterAppleLockCluster mtrClusterAppleLockCluster, IsMTRReadParams params) => mtrClusterAppleLockCluster -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterAppleLockCluster params =
  sendMessage mtrClusterAppleLockCluster readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterAppleLockCluster mtrClusterAppleLockCluster, IsMTRReadParams params) => mtrClusterAppleLockCluster -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterAppleLockCluster params =
  sendMessage mtrClusterAppleLockCluster readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterAppleLockCluster mtrClusterAppleLockCluster => mtrClusterAppleLockCluster -> IO (Id MTRClusterAppleLockCluster)
init_ mtrClusterAppleLockCluster =
  sendOwnedMessage mtrClusterAppleLockCluster initSelector

-- | @+ new@
new :: IO (Id MTRClusterAppleLockCluster)
new  =
  do
    cls' <- getRequiredClass "MTRClusterAppleLockCluster"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterAppleLockCluster mtrClusterAppleLockCluster, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterAppleLockCluster -> device -> endpointID -> queue -> IO (Id MTRClusterAppleLockCluster)
initWithDevice_endpointID_queue mtrClusterAppleLockCluster device endpointID queue =
  sendOwnedMessage mtrClusterAppleLockCluster initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeIntentDetectionWithParams:@
readAttributeIntentDetectionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeIntentDetectionWithParamsSelector = mkSelector "readAttributeIntentDetectionWithParams:"

-- | @Selector@ for @writeAttributeIntentDetectionWithValue:expectedValueInterval:@
writeAttributeIntentDetectionWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeIntentDetectionWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeIntentDetectionWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeIntentDetectionWithValue:expectedValueInterval:params:@
writeAttributeIntentDetectionWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeIntentDetectionWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeIntentDetectionWithValue:expectedValueInterval:params:"

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
initSelector :: Selector '[] (Id MTRClusterAppleLockCluster)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterAppleLockCluster)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterAppleLockCluster)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

