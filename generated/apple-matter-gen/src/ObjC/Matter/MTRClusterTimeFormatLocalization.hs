{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Time Format Localization    Nodes should be expected to be deployed to any and all regions of the world. These global regions      may have differing preferences for how dates and times are conveyed. As such, Nodes that visually      or audibly convey time information need a mechanism by which they can be configured to use a      user’s preferred format.
--
-- Generated bindings for @MTRClusterTimeFormatLocalization@.
module ObjC.Matter.MTRClusterTimeFormatLocalization
  ( MTRClusterTimeFormatLocalization
  , IsMTRClusterTimeFormatLocalization(..)
  , readAttributeHourFormatWithParams
  , writeAttributeHourFormatWithValue_expectedValueInterval
  , writeAttributeHourFormatWithValue_expectedValueInterval_params
  , readAttributeActiveCalendarTypeWithParams
  , writeAttributeActiveCalendarTypeWithValue_expectedValueInterval
  , writeAttributeActiveCalendarTypeWithValue_expectedValueInterval_params
  , readAttributeSupportedCalendarTypesWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpoint_queue
  , initWithDevice_endpointID_queue
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , initWithDevice_endpoint_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeActiveCalendarTypeWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeHourFormatWithParamsSelector
  , readAttributeSupportedCalendarTypesWithParamsSelector
  , writeAttributeActiveCalendarTypeWithValue_expectedValueIntervalSelector
  , writeAttributeActiveCalendarTypeWithValue_expectedValueInterval_paramsSelector
  , writeAttributeHourFormatWithValue_expectedValueIntervalSelector
  , writeAttributeHourFormatWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- readAttributeHourFormatWithParams:@
readAttributeHourFormatWithParams :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsMTRReadParams params) => mtrClusterTimeFormatLocalization -> params -> IO (Id NSDictionary)
readAttributeHourFormatWithParams mtrClusterTimeFormatLocalization params =
  sendMessage mtrClusterTimeFormatLocalization readAttributeHourFormatWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeHourFormatWithValue:expectedValueInterval:@
writeAttributeHourFormatWithValue_expectedValueInterval :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterTimeFormatLocalization -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeHourFormatWithValue_expectedValueInterval mtrClusterTimeFormatLocalization dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterTimeFormatLocalization writeAttributeHourFormatWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeHourFormatWithValue:expectedValueInterval:params:@
writeAttributeHourFormatWithValue_expectedValueInterval_params :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterTimeFormatLocalization -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeHourFormatWithValue_expectedValueInterval_params mtrClusterTimeFormatLocalization dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterTimeFormatLocalization writeAttributeHourFormatWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeActiveCalendarTypeWithParams:@
readAttributeActiveCalendarTypeWithParams :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsMTRReadParams params) => mtrClusterTimeFormatLocalization -> params -> IO (Id NSDictionary)
readAttributeActiveCalendarTypeWithParams mtrClusterTimeFormatLocalization params =
  sendMessage mtrClusterTimeFormatLocalization readAttributeActiveCalendarTypeWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeActiveCalendarTypeWithValue:expectedValueInterval:@
writeAttributeActiveCalendarTypeWithValue_expectedValueInterval :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterTimeFormatLocalization -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeActiveCalendarTypeWithValue_expectedValueInterval mtrClusterTimeFormatLocalization dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterTimeFormatLocalization writeAttributeActiveCalendarTypeWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeActiveCalendarTypeWithValue:expectedValueInterval:params:@
writeAttributeActiveCalendarTypeWithValue_expectedValueInterval_params :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterTimeFormatLocalization -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeActiveCalendarTypeWithValue_expectedValueInterval_params mtrClusterTimeFormatLocalization dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterTimeFormatLocalization writeAttributeActiveCalendarTypeWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeSupportedCalendarTypesWithParams:@
readAttributeSupportedCalendarTypesWithParams :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsMTRReadParams params) => mtrClusterTimeFormatLocalization -> params -> IO (Id NSDictionary)
readAttributeSupportedCalendarTypesWithParams mtrClusterTimeFormatLocalization params =
  sendMessage mtrClusterTimeFormatLocalization readAttributeSupportedCalendarTypesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsMTRReadParams params) => mtrClusterTimeFormatLocalization -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterTimeFormatLocalization params =
  sendMessage mtrClusterTimeFormatLocalization readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsMTRReadParams params) => mtrClusterTimeFormatLocalization -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterTimeFormatLocalization params =
  sendMessage mtrClusterTimeFormatLocalization readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsMTRReadParams params) => mtrClusterTimeFormatLocalization -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterTimeFormatLocalization params =
  sendMessage mtrClusterTimeFormatLocalization readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsMTRReadParams params) => mtrClusterTimeFormatLocalization -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterTimeFormatLocalization params =
  sendMessage mtrClusterTimeFormatLocalization readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsMTRReadParams params) => mtrClusterTimeFormatLocalization -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterTimeFormatLocalization params =
  sendMessage mtrClusterTimeFormatLocalization readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization => mtrClusterTimeFormatLocalization -> IO (Id MTRClusterTimeFormatLocalization)
init_ mtrClusterTimeFormatLocalization =
  sendOwnedMessage mtrClusterTimeFormatLocalization initSelector

-- | @+ new@
new :: IO (Id MTRClusterTimeFormatLocalization)
new  =
  do
    cls' <- getRequiredClass "MTRClusterTimeFormatLocalization"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queue :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsMTRDevice device, IsNSObject queue) => mtrClusterTimeFormatLocalization -> device -> CUShort -> queue -> IO (Id MTRClusterTimeFormatLocalization)
initWithDevice_endpoint_queue mtrClusterTimeFormatLocalization device endpoint queue =
  sendOwnedMessage mtrClusterTimeFormatLocalization initWithDevice_endpoint_queueSelector (toMTRDevice device) endpoint (toNSObject queue)

-- | The queue is currently unused, but may be used in the future for calling completions for command invocations if commands are added to this cluster.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterTimeFormatLocalization mtrClusterTimeFormatLocalization, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterTimeFormatLocalization -> device -> endpointID -> queue -> IO (Id MTRClusterTimeFormatLocalization)
initWithDevice_endpointID_queue mtrClusterTimeFormatLocalization device endpointID queue =
  sendOwnedMessage mtrClusterTimeFormatLocalization initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeHourFormatWithParams:@
readAttributeHourFormatWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeHourFormatWithParamsSelector = mkSelector "readAttributeHourFormatWithParams:"

-- | @Selector@ for @writeAttributeHourFormatWithValue:expectedValueInterval:@
writeAttributeHourFormatWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeHourFormatWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeHourFormatWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeHourFormatWithValue:expectedValueInterval:params:@
writeAttributeHourFormatWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeHourFormatWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeHourFormatWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeActiveCalendarTypeWithParams:@
readAttributeActiveCalendarTypeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeActiveCalendarTypeWithParamsSelector = mkSelector "readAttributeActiveCalendarTypeWithParams:"

-- | @Selector@ for @writeAttributeActiveCalendarTypeWithValue:expectedValueInterval:@
writeAttributeActiveCalendarTypeWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeActiveCalendarTypeWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeActiveCalendarTypeWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeActiveCalendarTypeWithValue:expectedValueInterval:params:@
writeAttributeActiveCalendarTypeWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeActiveCalendarTypeWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeActiveCalendarTypeWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeSupportedCalendarTypesWithParams:@
readAttributeSupportedCalendarTypesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportedCalendarTypesWithParamsSelector = mkSelector "readAttributeSupportedCalendarTypesWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterTimeFormatLocalization)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterTimeFormatLocalization)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpoint:queue:@
initWithDevice_endpoint_queueSelector :: Selector '[Id MTRDevice, CUShort, Id NSObject] (Id MTRClusterTimeFormatLocalization)
initWithDevice_endpoint_queueSelector = mkSelector "initWithDevice:endpoint:queue:"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterTimeFormatLocalization)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

