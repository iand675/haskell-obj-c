{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Energy Preference    This cluster provides an interface to specify preferences for how devices should consume energy.
--
-- Generated bindings for @MTRClusterEnergyPreference@.
module ObjC.Matter.MTRClusterEnergyPreference
  ( MTRClusterEnergyPreference
  , IsMTRClusterEnergyPreference(..)
  , readAttributeEnergyBalancesWithParams
  , readAttributeCurrentEnergyBalanceWithParams
  , writeAttributeCurrentEnergyBalanceWithValue_expectedValueInterval
  , writeAttributeCurrentEnergyBalanceWithValue_expectedValueInterval_params
  , readAttributeEnergyPrioritiesWithParams
  , readAttributeLowPowerModeSensitivitiesWithParams
  , readAttributeCurrentLowPowerModeSensitivityWithParams
  , writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueInterval
  , writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueInterval_params
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
  , readAttributeCurrentEnergyBalanceWithParamsSelector
  , readAttributeCurrentLowPowerModeSensitivityWithParamsSelector
  , readAttributeEnergyBalancesWithParamsSelector
  , readAttributeEnergyPrioritiesWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeLowPowerModeSensitivitiesWithParamsSelector
  , writeAttributeCurrentEnergyBalanceWithValue_expectedValueIntervalSelector
  , writeAttributeCurrentEnergyBalanceWithValue_expectedValueInterval_paramsSelector
  , writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueIntervalSelector
  , writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeEnergyBalancesWithParams:@
readAttributeEnergyBalancesWithParams :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsMTRReadParams params) => mtrClusterEnergyPreference -> params -> IO (Id NSDictionary)
readAttributeEnergyBalancesWithParams mtrClusterEnergyPreference params =
  sendMessage mtrClusterEnergyPreference readAttributeEnergyBalancesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentEnergyBalanceWithParams:@
readAttributeCurrentEnergyBalanceWithParams :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsMTRReadParams params) => mtrClusterEnergyPreference -> params -> IO (Id NSDictionary)
readAttributeCurrentEnergyBalanceWithParams mtrClusterEnergyPreference params =
  sendMessage mtrClusterEnergyPreference readAttributeCurrentEnergyBalanceWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeCurrentEnergyBalanceWithValue:expectedValueInterval:@
writeAttributeCurrentEnergyBalanceWithValue_expectedValueInterval :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyPreference -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeCurrentEnergyBalanceWithValue_expectedValueInterval mtrClusterEnergyPreference dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterEnergyPreference writeAttributeCurrentEnergyBalanceWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeCurrentEnergyBalanceWithValue:expectedValueInterval:params:@
writeAttributeCurrentEnergyBalanceWithValue_expectedValueInterval_params :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterEnergyPreference -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeCurrentEnergyBalanceWithValue_expectedValueInterval_params mtrClusterEnergyPreference dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterEnergyPreference writeAttributeCurrentEnergyBalanceWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeEnergyPrioritiesWithParams:@
readAttributeEnergyPrioritiesWithParams :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsMTRReadParams params) => mtrClusterEnergyPreference -> params -> IO (Id NSDictionary)
readAttributeEnergyPrioritiesWithParams mtrClusterEnergyPreference params =
  sendMessage mtrClusterEnergyPreference readAttributeEnergyPrioritiesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLowPowerModeSensitivitiesWithParams:@
readAttributeLowPowerModeSensitivitiesWithParams :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsMTRReadParams params) => mtrClusterEnergyPreference -> params -> IO (Id NSDictionary)
readAttributeLowPowerModeSensitivitiesWithParams mtrClusterEnergyPreference params =
  sendMessage mtrClusterEnergyPreference readAttributeLowPowerModeSensitivitiesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentLowPowerModeSensitivityWithParams:@
readAttributeCurrentLowPowerModeSensitivityWithParams :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsMTRReadParams params) => mtrClusterEnergyPreference -> params -> IO (Id NSDictionary)
readAttributeCurrentLowPowerModeSensitivityWithParams mtrClusterEnergyPreference params =
  sendMessage mtrClusterEnergyPreference readAttributeCurrentLowPowerModeSensitivityWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeCurrentLowPowerModeSensitivityWithValue:expectedValueInterval:@
writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueInterval :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterEnergyPreference -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueInterval mtrClusterEnergyPreference dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterEnergyPreference writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeCurrentLowPowerModeSensitivityWithValue:expectedValueInterval:params:@
writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueInterval_params :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterEnergyPreference -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueInterval_params mtrClusterEnergyPreference dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterEnergyPreference writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsMTRReadParams params) => mtrClusterEnergyPreference -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterEnergyPreference params =
  sendMessage mtrClusterEnergyPreference readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsMTRReadParams params) => mtrClusterEnergyPreference -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterEnergyPreference params =
  sendMessage mtrClusterEnergyPreference readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsMTRReadParams params) => mtrClusterEnergyPreference -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterEnergyPreference params =
  sendMessage mtrClusterEnergyPreference readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsMTRReadParams params) => mtrClusterEnergyPreference -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterEnergyPreference params =
  sendMessage mtrClusterEnergyPreference readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsMTRReadParams params) => mtrClusterEnergyPreference -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterEnergyPreference params =
  sendMessage mtrClusterEnergyPreference readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterEnergyPreference mtrClusterEnergyPreference => mtrClusterEnergyPreference -> IO (Id MTRClusterEnergyPreference)
init_ mtrClusterEnergyPreference =
  sendOwnedMessage mtrClusterEnergyPreference initSelector

-- | @+ new@
new :: IO (Id MTRClusterEnergyPreference)
new  =
  do
    cls' <- getRequiredClass "MTRClusterEnergyPreference"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterEnergyPreference mtrClusterEnergyPreference, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterEnergyPreference -> device -> endpointID -> queue -> IO (Id MTRClusterEnergyPreference)
initWithDevice_endpointID_queue mtrClusterEnergyPreference device endpointID queue =
  sendOwnedMessage mtrClusterEnergyPreference initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeEnergyBalancesWithParams:@
readAttributeEnergyBalancesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeEnergyBalancesWithParamsSelector = mkSelector "readAttributeEnergyBalancesWithParams:"

-- | @Selector@ for @readAttributeCurrentEnergyBalanceWithParams:@
readAttributeCurrentEnergyBalanceWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentEnergyBalanceWithParamsSelector = mkSelector "readAttributeCurrentEnergyBalanceWithParams:"

-- | @Selector@ for @writeAttributeCurrentEnergyBalanceWithValue:expectedValueInterval:@
writeAttributeCurrentEnergyBalanceWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeCurrentEnergyBalanceWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeCurrentEnergyBalanceWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeCurrentEnergyBalanceWithValue:expectedValueInterval:params:@
writeAttributeCurrentEnergyBalanceWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeCurrentEnergyBalanceWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeCurrentEnergyBalanceWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeEnergyPrioritiesWithParams:@
readAttributeEnergyPrioritiesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeEnergyPrioritiesWithParamsSelector = mkSelector "readAttributeEnergyPrioritiesWithParams:"

-- | @Selector@ for @readAttributeLowPowerModeSensitivitiesWithParams:@
readAttributeLowPowerModeSensitivitiesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLowPowerModeSensitivitiesWithParamsSelector = mkSelector "readAttributeLowPowerModeSensitivitiesWithParams:"

-- | @Selector@ for @readAttributeCurrentLowPowerModeSensitivityWithParams:@
readAttributeCurrentLowPowerModeSensitivityWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentLowPowerModeSensitivityWithParamsSelector = mkSelector "readAttributeCurrentLowPowerModeSensitivityWithParams:"

-- | @Selector@ for @writeAttributeCurrentLowPowerModeSensitivityWithValue:expectedValueInterval:@
writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeCurrentLowPowerModeSensitivityWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeCurrentLowPowerModeSensitivityWithValue:expectedValueInterval:params:@
writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeCurrentLowPowerModeSensitivityWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeCurrentLowPowerModeSensitivityWithValue:expectedValueInterval:params:"

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
initSelector :: Selector '[] (Id MTRClusterEnergyPreference)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterEnergyPreference)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterEnergyPreference)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

