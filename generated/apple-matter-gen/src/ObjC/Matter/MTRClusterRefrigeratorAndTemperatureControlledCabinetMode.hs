{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Refrigerator And Temperature Controlled Cabinet Mode    Attributes and commands for selecting a mode from a list of supported options.
--
-- Generated bindings for @MTRClusterRefrigeratorAndTemperatureControlledCabinetMode@.
module ObjC.Matter.MTRClusterRefrigeratorAndTemperatureControlledCabinetMode
  ( MTRClusterRefrigeratorAndTemperatureControlledCabinetMode
  , IsMTRClusterRefrigeratorAndTemperatureControlledCabinetMode(..)
  , changeToModeWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeSupportedModesWithParams
  , readAttributeCurrentModeWithParams
  , readAttributeGeneratedCommandListWithParams
  , readAttributeAcceptedCommandListWithParams
  , readAttributeAttributeListWithParams
  , readAttributeFeatureMapWithParams
  , readAttributeClusterRevisionWithParams
  , init_
  , new
  , initWithDevice_endpointID_queue
  , changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector
  , initSelector
  , initWithDevice_endpointID_queueSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeCurrentModeWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeSupportedModesWithParamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- changeToModeWithParams:expectedValues:expectedValueInterval:completion:@
changeToModeWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterRefrigeratorAndTemperatureControlledCabinetMode mtrClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
changeToModeWithParams_expectedValues_expectedValueInterval_completion mtrClusterRefrigeratorAndTemperatureControlledCabinetMode params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterRefrigeratorAndTemperatureControlledCabinetMode changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeSupportedModesWithParams:@
readAttributeSupportedModesWithParams :: (IsMTRClusterRefrigeratorAndTemperatureControlledCabinetMode mtrClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRReadParams params) => mtrClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> IO (Id NSDictionary)
readAttributeSupportedModesWithParams mtrClusterRefrigeratorAndTemperatureControlledCabinetMode params =
  sendMessage mtrClusterRefrigeratorAndTemperatureControlledCabinetMode readAttributeSupportedModesWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCurrentModeWithParams:@
readAttributeCurrentModeWithParams :: (IsMTRClusterRefrigeratorAndTemperatureControlledCabinetMode mtrClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRReadParams params) => mtrClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> IO (Id NSDictionary)
readAttributeCurrentModeWithParams mtrClusterRefrigeratorAndTemperatureControlledCabinetMode params =
  sendMessage mtrClusterRefrigeratorAndTemperatureControlledCabinetMode readAttributeCurrentModeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterRefrigeratorAndTemperatureControlledCabinetMode mtrClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRReadParams params) => mtrClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterRefrigeratorAndTemperatureControlledCabinetMode params =
  sendMessage mtrClusterRefrigeratorAndTemperatureControlledCabinetMode readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterRefrigeratorAndTemperatureControlledCabinetMode mtrClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRReadParams params) => mtrClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterRefrigeratorAndTemperatureControlledCabinetMode params =
  sendMessage mtrClusterRefrigeratorAndTemperatureControlledCabinetMode readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterRefrigeratorAndTemperatureControlledCabinetMode mtrClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRReadParams params) => mtrClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterRefrigeratorAndTemperatureControlledCabinetMode params =
  sendMessage mtrClusterRefrigeratorAndTemperatureControlledCabinetMode readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterRefrigeratorAndTemperatureControlledCabinetMode mtrClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRReadParams params) => mtrClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterRefrigeratorAndTemperatureControlledCabinetMode params =
  sendMessage mtrClusterRefrigeratorAndTemperatureControlledCabinetMode readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterRefrigeratorAndTemperatureControlledCabinetMode mtrClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRReadParams params) => mtrClusterRefrigeratorAndTemperatureControlledCabinetMode -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterRefrigeratorAndTemperatureControlledCabinetMode params =
  sendMessage mtrClusterRefrigeratorAndTemperatureControlledCabinetMode readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterRefrigeratorAndTemperatureControlledCabinetMode mtrClusterRefrigeratorAndTemperatureControlledCabinetMode => mtrClusterRefrigeratorAndTemperatureControlledCabinetMode -> IO (Id MTRClusterRefrigeratorAndTemperatureControlledCabinetMode)
init_ mtrClusterRefrigeratorAndTemperatureControlledCabinetMode =
  sendOwnedMessage mtrClusterRefrigeratorAndTemperatureControlledCabinetMode initSelector

-- | @+ new@
new :: IO (Id MTRClusterRefrigeratorAndTemperatureControlledCabinetMode)
new  =
  do
    cls' <- getRequiredClass "MTRClusterRefrigeratorAndTemperatureControlledCabinetMode"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterRefrigeratorAndTemperatureControlledCabinetMode mtrClusterRefrigeratorAndTemperatureControlledCabinetMode, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterRefrigeratorAndTemperatureControlledCabinetMode -> device -> endpointID -> queue -> IO (Id MTRClusterRefrigeratorAndTemperatureControlledCabinetMode)
initWithDevice_endpointID_queue mtrClusterRefrigeratorAndTemperatureControlledCabinetMode device endpointID queue =
  sendOwnedMessage mtrClusterRefrigeratorAndTemperatureControlledCabinetMode initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @changeToModeWithParams:expectedValues:expectedValueInterval:completion:@
changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRRefrigeratorAndTemperatureControlledCabinetModeClusterChangeToModeParams, Id NSArray, Id NSNumber, Ptr ()] ()
changeToModeWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "changeToModeWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeSupportedModesWithParams:@
readAttributeSupportedModesWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSupportedModesWithParamsSelector = mkSelector "readAttributeSupportedModesWithParams:"

-- | @Selector@ for @readAttributeCurrentModeWithParams:@
readAttributeCurrentModeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCurrentModeWithParamsSelector = mkSelector "readAttributeCurrentModeWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterRefrigeratorAndTemperatureControlledCabinetMode)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterRefrigeratorAndTemperatureControlledCabinetMode)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterRefrigeratorAndTemperatureControlledCabinetMode)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

