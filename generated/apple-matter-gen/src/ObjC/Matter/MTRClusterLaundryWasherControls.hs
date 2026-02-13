{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Laundry Washer Controls    This cluster supports remotely monitoring and controlling the different types of functionality available to a washing device, such as a washing machine.
--
-- Generated bindings for @MTRClusterLaundryWasherControls@.
module ObjC.Matter.MTRClusterLaundryWasherControls
  ( MTRClusterLaundryWasherControls
  , IsMTRClusterLaundryWasherControls(..)
  , readAttributeSpinSpeedsWithParams
  , readAttributeSpinSpeedCurrentWithParams
  , writeAttributeSpinSpeedCurrentWithValue_expectedValueInterval
  , writeAttributeSpinSpeedCurrentWithValue_expectedValueInterval_params
  , readAttributeNumberOfRinsesWithParams
  , writeAttributeNumberOfRinsesWithValue_expectedValueInterval
  , writeAttributeNumberOfRinsesWithValue_expectedValueInterval_params
  , readAttributeSupportedRinsesWithParams
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
  , readAttributeNumberOfRinsesWithParamsSelector
  , readAttributeSpinSpeedCurrentWithParamsSelector
  , readAttributeSpinSpeedsWithParamsSelector
  , readAttributeSupportedRinsesWithParamsSelector
  , writeAttributeNumberOfRinsesWithValue_expectedValueIntervalSelector
  , writeAttributeNumberOfRinsesWithValue_expectedValueInterval_paramsSelector
  , writeAttributeSpinSpeedCurrentWithValue_expectedValueIntervalSelector
  , writeAttributeSpinSpeedCurrentWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeSpinSpeedsWithParams:@
readAttributeSpinSpeedsWithParams :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsMTRReadParams params) => mtrClusterLaundryWasherControls -> params -> IO (Id NSDictionary)
readAttributeSpinSpeedsWithParams mtrClusterLaundryWasherControls params =
  sendMessage mtrClusterLaundryWasherControls readAttributeSpinSpeedsWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSpinSpeedCurrentWithParams:@
readAttributeSpinSpeedCurrentWithParams :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsMTRReadParams params) => mtrClusterLaundryWasherControls -> params -> IO (Id NSDictionary)
readAttributeSpinSpeedCurrentWithParams mtrClusterLaundryWasherControls params =
  sendMessage mtrClusterLaundryWasherControls readAttributeSpinSpeedCurrentWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeSpinSpeedCurrentWithValue:expectedValueInterval:@
writeAttributeSpinSpeedCurrentWithValue_expectedValueInterval :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterLaundryWasherControls -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeSpinSpeedCurrentWithValue_expectedValueInterval mtrClusterLaundryWasherControls dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterLaundryWasherControls writeAttributeSpinSpeedCurrentWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeSpinSpeedCurrentWithValue:expectedValueInterval:params:@
writeAttributeSpinSpeedCurrentWithValue_expectedValueInterval_params :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterLaundryWasherControls -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeSpinSpeedCurrentWithValue_expectedValueInterval_params mtrClusterLaundryWasherControls dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterLaundryWasherControls writeAttributeSpinSpeedCurrentWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeNumberOfRinsesWithParams:@
readAttributeNumberOfRinsesWithParams :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsMTRReadParams params) => mtrClusterLaundryWasherControls -> params -> IO (Id NSDictionary)
readAttributeNumberOfRinsesWithParams mtrClusterLaundryWasherControls params =
  sendMessage mtrClusterLaundryWasherControls readAttributeNumberOfRinsesWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeNumberOfRinsesWithValue:expectedValueInterval:@
writeAttributeNumberOfRinsesWithValue_expectedValueInterval :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterLaundryWasherControls -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeNumberOfRinsesWithValue_expectedValueInterval mtrClusterLaundryWasherControls dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterLaundryWasherControls writeAttributeNumberOfRinsesWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeNumberOfRinsesWithValue:expectedValueInterval:params:@
writeAttributeNumberOfRinsesWithValue_expectedValueInterval_params :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterLaundryWasherControls -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeNumberOfRinsesWithValue_expectedValueInterval_params mtrClusterLaundryWasherControls dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterLaundryWasherControls writeAttributeNumberOfRinsesWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeSupportedRinsesWithParams:@
readAttributeSupportedRinsesWithParams :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsMTRReadParams params) => mtrClusterLaundryWasherControls -> params -> IO (Id NSDictionary)
readAttributeSupportedRinsesWithParams mtrClusterLaundryWasherControls params =
  sendMessage mtrClusterLaundryWasherControls readAttributeSupportedRinsesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsMTRReadParams params) => mtrClusterLaundryWasherControls -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterLaundryWasherControls params =
  sendMessage mtrClusterLaundryWasherControls readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsMTRReadParams params) => mtrClusterLaundryWasherControls -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterLaundryWasherControls params =
  sendMessage mtrClusterLaundryWasherControls readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsMTRReadParams params) => mtrClusterLaundryWasherControls -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterLaundryWasherControls params =
  sendMessage mtrClusterLaundryWasherControls readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsMTRReadParams params) => mtrClusterLaundryWasherControls -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterLaundryWasherControls params =
  sendMessage mtrClusterLaundryWasherControls readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsMTRReadParams params) => mtrClusterLaundryWasherControls -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterLaundryWasherControls params =
  sendMessage mtrClusterLaundryWasherControls readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls => mtrClusterLaundryWasherControls -> IO (Id MTRClusterLaundryWasherControls)
init_ mtrClusterLaundryWasherControls =
  sendOwnedMessage mtrClusterLaundryWasherControls initSelector

-- | @+ new@
new :: IO (Id MTRClusterLaundryWasherControls)
new  =
  do
    cls' <- getRequiredClass "MTRClusterLaundryWasherControls"
    sendOwnedClassMessage cls' newSelector

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterLaundryWasherControls mtrClusterLaundryWasherControls, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterLaundryWasherControls -> device -> endpointID -> queue -> IO (Id MTRClusterLaundryWasherControls)
initWithDevice_endpointID_queue mtrClusterLaundryWasherControls device endpointID queue =
  sendOwnedMessage mtrClusterLaundryWasherControls initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeSpinSpeedsWithParams:@
readAttributeSpinSpeedsWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSpinSpeedsWithParamsSelector = mkSelector "readAttributeSpinSpeedsWithParams:"

-- | @Selector@ for @readAttributeSpinSpeedCurrentWithParams:@
readAttributeSpinSpeedCurrentWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSpinSpeedCurrentWithParamsSelector = mkSelector "readAttributeSpinSpeedCurrentWithParams:"

-- | @Selector@ for @writeAttributeSpinSpeedCurrentWithValue:expectedValueInterval:@
writeAttributeSpinSpeedCurrentWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeSpinSpeedCurrentWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeSpinSpeedCurrentWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeSpinSpeedCurrentWithValue:expectedValueInterval:params:@
writeAttributeSpinSpeedCurrentWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeSpinSpeedCurrentWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeSpinSpeedCurrentWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeNumberOfRinsesWithParams:@
readAttributeNumberOfRinsesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNumberOfRinsesWithParamsSelector = mkSelector "readAttributeNumberOfRinsesWithParams:"

-- | @Selector@ for @writeAttributeNumberOfRinsesWithValue:expectedValueInterval:@
writeAttributeNumberOfRinsesWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeNumberOfRinsesWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeNumberOfRinsesWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeNumberOfRinsesWithValue:expectedValueInterval:params:@
writeAttributeNumberOfRinsesWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeNumberOfRinsesWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeNumberOfRinsesWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSupportedRinsesWithParams:@
readAttributeSupportedRinsesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportedRinsesWithParamsSelector = mkSelector "readAttributeSupportedRinsesWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterLaundryWasherControls)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterLaundryWasherControls)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterLaundryWasherControls)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

