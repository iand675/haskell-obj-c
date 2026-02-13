{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Bridged Device Basic Information    This Cluster serves two purposes towards a Node communicating with a Bridge: indicate that the functionality on          the Endpoint where it is placed (and its Parts) is bridged from a non-CHIP technology; and provide a centralized          collection of attributes that the Node MAY collect to aid in conveying information regarding the Bridged Device to a user,          such as the vendor name, the model name, or user-assigned name.
--
-- Generated bindings for @MTRClusterBridgedDeviceBasicInformation@.
module ObjC.Matter.MTRClusterBridgedDeviceBasicInformation
  ( MTRClusterBridgedDeviceBasicInformation
  , IsMTRClusterBridgedDeviceBasicInformation(..)
  , keepActiveWithParams_expectedValues_expectedValueInterval_completion
  , readAttributeVendorNameWithParams
  , readAttributeVendorIDWithParams
  , readAttributeProductNameWithParams
  , readAttributeProductIDWithParams
  , readAttributeNodeLabelWithParams
  , writeAttributeNodeLabelWithValue_expectedValueInterval
  , writeAttributeNodeLabelWithValue_expectedValueInterval_params
  , readAttributeHardwareVersionWithParams
  , readAttributeHardwareVersionStringWithParams
  , readAttributeSoftwareVersionWithParams
  , readAttributeSoftwareVersionStringWithParams
  , readAttributeManufacturingDateWithParams
  , readAttributePartNumberWithParams
  , readAttributeProductURLWithParams
  , readAttributeProductLabelWithParams
  , readAttributeSerialNumberWithParams
  , readAttributeReachableWithParams
  , readAttributeUniqueIDWithParams
  , readAttributeProductAppearanceWithParams
  , readAttributeConfigurationVersionWithParams
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
  , keepActiveWithParams_expectedValues_expectedValueInterval_completionSelector
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeConfigurationVersionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeHardwareVersionStringWithParamsSelector
  , readAttributeHardwareVersionWithParamsSelector
  , readAttributeManufacturingDateWithParamsSelector
  , readAttributeNodeLabelWithParamsSelector
  , readAttributePartNumberWithParamsSelector
  , readAttributeProductAppearanceWithParamsSelector
  , readAttributeProductIDWithParamsSelector
  , readAttributeProductLabelWithParamsSelector
  , readAttributeProductNameWithParamsSelector
  , readAttributeProductURLWithParamsSelector
  , readAttributeReachableWithParamsSelector
  , readAttributeSerialNumberWithParamsSelector
  , readAttributeSoftwareVersionStringWithParamsSelector
  , readAttributeSoftwareVersionWithParamsSelector
  , readAttributeUniqueIDWithParamsSelector
  , readAttributeVendorIDWithParamsSelector
  , readAttributeVendorNameWithParamsSelector
  , writeAttributeNodeLabelWithValue_expectedValueIntervalSelector
  , writeAttributeNodeLabelWithValue_expectedValueInterval_paramsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- keepActiveWithParams:expectedValues:expectedValueInterval:completion:@
keepActiveWithParams_expectedValues_expectedValueInterval_completion :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRBridgedDeviceBasicInformationClusterKeepActiveParams params, IsNSArray expectedDataValueDictionaries, IsNSNumber expectedValueIntervalMs) => mtrClusterBridgedDeviceBasicInformation -> params -> expectedDataValueDictionaries -> expectedValueIntervalMs -> Ptr () -> IO ()
keepActiveWithParams_expectedValues_expectedValueInterval_completion mtrClusterBridgedDeviceBasicInformation params expectedDataValueDictionaries expectedValueIntervalMs completion =
  sendMessage mtrClusterBridgedDeviceBasicInformation keepActiveWithParams_expectedValues_expectedValueInterval_completionSelector (toMTRBridgedDeviceBasicInformationClusterKeepActiveParams params) (toNSArray expectedDataValueDictionaries) (toNSNumber expectedValueIntervalMs) completion

-- | @- readAttributeVendorNameWithParams:@
readAttributeVendorNameWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeVendorNameWithParams mtrClusterBridgedDeviceBasicInformation params =
  sendMessage mtrClusterBridgedDeviceBasicInformation readAttributeVendorNameWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeVendorIDWithParams:@
readAttributeVendorIDWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeVendorIDWithParams mtrClusterBridgedDeviceBasicInformation params =
  sendMessage mtrClusterBridgedDeviceBasicInformation readAttributeVendorIDWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeProductNameWithParams:@
readAttributeProductNameWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeProductNameWithParams mtrClusterBridgedDeviceBasicInformation params =
  sendMessage mtrClusterBridgedDeviceBasicInformation readAttributeProductNameWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeProductIDWithParams:@
readAttributeProductIDWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeProductIDWithParams mtrClusterBridgedDeviceBasicInformation params =
  sendMessage mtrClusterBridgedDeviceBasicInformation readAttributeProductIDWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeNodeLabelWithParams:@
readAttributeNodeLabelWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeNodeLabelWithParams mtrClusterBridgedDeviceBasicInformation params =
  sendMessage mtrClusterBridgedDeviceBasicInformation readAttributeNodeLabelWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeNodeLabelWithValue:expectedValueInterval:@
writeAttributeNodeLabelWithValue_expectedValueInterval :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBridgedDeviceBasicInformation -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeNodeLabelWithValue_expectedValueInterval mtrClusterBridgedDeviceBasicInformation dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBridgedDeviceBasicInformation writeAttributeNodeLabelWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeNodeLabelWithValue:expectedValueInterval:params:@
writeAttributeNodeLabelWithValue_expectedValueInterval_params :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBridgedDeviceBasicInformation -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeNodeLabelWithValue_expectedValueInterval_params mtrClusterBridgedDeviceBasicInformation dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBridgedDeviceBasicInformation writeAttributeNodeLabelWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeHardwareVersionWithParams:@
readAttributeHardwareVersionWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeHardwareVersionWithParams mtrClusterBridgedDeviceBasicInformation params =
  sendMessage mtrClusterBridgedDeviceBasicInformation readAttributeHardwareVersionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeHardwareVersionStringWithParams:@
readAttributeHardwareVersionStringWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeHardwareVersionStringWithParams mtrClusterBridgedDeviceBasicInformation params =
  sendMessage mtrClusterBridgedDeviceBasicInformation readAttributeHardwareVersionStringWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSoftwareVersionWithParams:@
readAttributeSoftwareVersionWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeSoftwareVersionWithParams mtrClusterBridgedDeviceBasicInformation params =
  sendMessage mtrClusterBridgedDeviceBasicInformation readAttributeSoftwareVersionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSoftwareVersionStringWithParams:@
readAttributeSoftwareVersionStringWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeSoftwareVersionStringWithParams mtrClusterBridgedDeviceBasicInformation params =
  sendMessage mtrClusterBridgedDeviceBasicInformation readAttributeSoftwareVersionStringWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeManufacturingDateWithParams:@
readAttributeManufacturingDateWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeManufacturingDateWithParams mtrClusterBridgedDeviceBasicInformation params =
  sendMessage mtrClusterBridgedDeviceBasicInformation readAttributeManufacturingDateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePartNumberWithParams:@
readAttributePartNumberWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributePartNumberWithParams mtrClusterBridgedDeviceBasicInformation params =
  sendMessage mtrClusterBridgedDeviceBasicInformation readAttributePartNumberWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeProductURLWithParams:@
readAttributeProductURLWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeProductURLWithParams mtrClusterBridgedDeviceBasicInformation params =
  sendMessage mtrClusterBridgedDeviceBasicInformation readAttributeProductURLWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeProductLabelWithParams:@
readAttributeProductLabelWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeProductLabelWithParams mtrClusterBridgedDeviceBasicInformation params =
  sendMessage mtrClusterBridgedDeviceBasicInformation readAttributeProductLabelWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSerialNumberWithParams:@
readAttributeSerialNumberWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeSerialNumberWithParams mtrClusterBridgedDeviceBasicInformation params =
  sendMessage mtrClusterBridgedDeviceBasicInformation readAttributeSerialNumberWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeReachableWithParams:@
readAttributeReachableWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeReachableWithParams mtrClusterBridgedDeviceBasicInformation params =
  sendMessage mtrClusterBridgedDeviceBasicInformation readAttributeReachableWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeUniqueIDWithParams:@
readAttributeUniqueIDWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeUniqueIDWithParams mtrClusterBridgedDeviceBasicInformation params =
  sendMessage mtrClusterBridgedDeviceBasicInformation readAttributeUniqueIDWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeProductAppearanceWithParams:@
readAttributeProductAppearanceWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeProductAppearanceWithParams mtrClusterBridgedDeviceBasicInformation params =
  sendMessage mtrClusterBridgedDeviceBasicInformation readAttributeProductAppearanceWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeConfigurationVersionWithParams:@
readAttributeConfigurationVersionWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeConfigurationVersionWithParams mtrClusterBridgedDeviceBasicInformation params =
  sendMessage mtrClusterBridgedDeviceBasicInformation readAttributeConfigurationVersionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterBridgedDeviceBasicInformation params =
  sendMessage mtrClusterBridgedDeviceBasicInformation readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterBridgedDeviceBasicInformation params =
  sendMessage mtrClusterBridgedDeviceBasicInformation readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterBridgedDeviceBasicInformation params =
  sendMessage mtrClusterBridgedDeviceBasicInformation readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterBridgedDeviceBasicInformation params =
  sendMessage mtrClusterBridgedDeviceBasicInformation readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRReadParams params) => mtrClusterBridgedDeviceBasicInformation -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterBridgedDeviceBasicInformation params =
  sendMessage mtrClusterBridgedDeviceBasicInformation readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation => mtrClusterBridgedDeviceBasicInformation -> IO (Id MTRClusterBridgedDeviceBasicInformation)
init_ mtrClusterBridgedDeviceBasicInformation =
  sendOwnedMessage mtrClusterBridgedDeviceBasicInformation initSelector

-- | @+ new@
new :: IO (Id MTRClusterBridgedDeviceBasicInformation)
new  =
  do
    cls' <- getRequiredClass "MTRClusterBridgedDeviceBasicInformation"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterBridgedDeviceBasicInformation mtrClusterBridgedDeviceBasicInformation, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterBridgedDeviceBasicInformation -> device -> endpointID -> queue -> IO (Id MTRClusterBridgedDeviceBasicInformation)
initWithDevice_endpointID_queue mtrClusterBridgedDeviceBasicInformation device endpointID queue =
  sendOwnedMessage mtrClusterBridgedDeviceBasicInformation initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @keepActiveWithParams:expectedValues:expectedValueInterval:completion:@
keepActiveWithParams_expectedValues_expectedValueInterval_completionSelector :: Selector '[Id MTRBridgedDeviceBasicInformationClusterKeepActiveParams, Id NSArray, Id NSNumber, Ptr ()] ()
keepActiveWithParams_expectedValues_expectedValueInterval_completionSelector = mkSelector "keepActiveWithParams:expectedValues:expectedValueInterval:completion:"

-- | @Selector@ for @readAttributeVendorNameWithParams:@
readAttributeVendorNameWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeVendorNameWithParamsSelector = mkSelector "readAttributeVendorNameWithParams:"

-- | @Selector@ for @readAttributeVendorIDWithParams:@
readAttributeVendorIDWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeVendorIDWithParamsSelector = mkSelector "readAttributeVendorIDWithParams:"

-- | @Selector@ for @readAttributeProductNameWithParams:@
readAttributeProductNameWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeProductNameWithParamsSelector = mkSelector "readAttributeProductNameWithParams:"

-- | @Selector@ for @readAttributeProductIDWithParams:@
readAttributeProductIDWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeProductIDWithParamsSelector = mkSelector "readAttributeProductIDWithParams:"

-- | @Selector@ for @readAttributeNodeLabelWithParams:@
readAttributeNodeLabelWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeNodeLabelWithParamsSelector = mkSelector "readAttributeNodeLabelWithParams:"

-- | @Selector@ for @writeAttributeNodeLabelWithValue:expectedValueInterval:@
writeAttributeNodeLabelWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeNodeLabelWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeNodeLabelWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeNodeLabelWithValue:expectedValueInterval:params:@
writeAttributeNodeLabelWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeNodeLabelWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeNodeLabelWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeHardwareVersionWithParams:@
readAttributeHardwareVersionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeHardwareVersionWithParamsSelector = mkSelector "readAttributeHardwareVersionWithParams:"

-- | @Selector@ for @readAttributeHardwareVersionStringWithParams:@
readAttributeHardwareVersionStringWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeHardwareVersionStringWithParamsSelector = mkSelector "readAttributeHardwareVersionStringWithParams:"

-- | @Selector@ for @readAttributeSoftwareVersionWithParams:@
readAttributeSoftwareVersionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSoftwareVersionWithParamsSelector = mkSelector "readAttributeSoftwareVersionWithParams:"

-- | @Selector@ for @readAttributeSoftwareVersionStringWithParams:@
readAttributeSoftwareVersionStringWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSoftwareVersionStringWithParamsSelector = mkSelector "readAttributeSoftwareVersionStringWithParams:"

-- | @Selector@ for @readAttributeManufacturingDateWithParams:@
readAttributeManufacturingDateWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeManufacturingDateWithParamsSelector = mkSelector "readAttributeManufacturingDateWithParams:"

-- | @Selector@ for @readAttributePartNumberWithParams:@
readAttributePartNumberWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributePartNumberWithParamsSelector = mkSelector "readAttributePartNumberWithParams:"

-- | @Selector@ for @readAttributeProductURLWithParams:@
readAttributeProductURLWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeProductURLWithParamsSelector = mkSelector "readAttributeProductURLWithParams:"

-- | @Selector@ for @readAttributeProductLabelWithParams:@
readAttributeProductLabelWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeProductLabelWithParamsSelector = mkSelector "readAttributeProductLabelWithParams:"

-- | @Selector@ for @readAttributeSerialNumberWithParams:@
readAttributeSerialNumberWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSerialNumberWithParamsSelector = mkSelector "readAttributeSerialNumberWithParams:"

-- | @Selector@ for @readAttributeReachableWithParams:@
readAttributeReachableWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeReachableWithParamsSelector = mkSelector "readAttributeReachableWithParams:"

-- | @Selector@ for @readAttributeUniqueIDWithParams:@
readAttributeUniqueIDWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeUniqueIDWithParamsSelector = mkSelector "readAttributeUniqueIDWithParams:"

-- | @Selector@ for @readAttributeProductAppearanceWithParams:@
readAttributeProductAppearanceWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeProductAppearanceWithParamsSelector = mkSelector "readAttributeProductAppearanceWithParams:"

-- | @Selector@ for @readAttributeConfigurationVersionWithParams:@
readAttributeConfigurationVersionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeConfigurationVersionWithParamsSelector = mkSelector "readAttributeConfigurationVersionWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterBridgedDeviceBasicInformation)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterBridgedDeviceBasicInformation)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterBridgedDeviceBasicInformation)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

