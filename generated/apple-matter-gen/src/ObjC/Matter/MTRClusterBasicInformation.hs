{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Cluster Basic Information    This cluster provides attributes and events for determining basic information about Nodes, which supports both      Commissioning and operational determination of Node characteristics, such as Vendor ID, Product ID and serial number,      which apply to the whole Node. Also allows setting user device information such as location.
--
-- Generated bindings for @MTRClusterBasicInformation@.
module ObjC.Matter.MTRClusterBasicInformation
  ( MTRClusterBasicInformation
  , IsMTRClusterBasicInformation(..)
  , readAttributeDataModelRevisionWithParams
  , readAttributeVendorNameWithParams
  , readAttributeVendorIDWithParams
  , readAttributeProductNameWithParams
  , readAttributeProductIDWithParams
  , readAttributeNodeLabelWithParams
  , writeAttributeNodeLabelWithValue_expectedValueInterval
  , writeAttributeNodeLabelWithValue_expectedValueInterval_params
  , readAttributeLocationWithParams
  , writeAttributeLocationWithValue_expectedValueInterval
  , writeAttributeLocationWithValue_expectedValueInterval_params
  , readAttributeHardwareVersionWithParams
  , readAttributeHardwareVersionStringWithParams
  , readAttributeSoftwareVersionWithParams
  , readAttributeSoftwareVersionStringWithParams
  , readAttributeManufacturingDateWithParams
  , readAttributePartNumberWithParams
  , readAttributeProductURLWithParams
  , readAttributeProductLabelWithParams
  , readAttributeSerialNumberWithParams
  , readAttributeLocalConfigDisabledWithParams
  , writeAttributeLocalConfigDisabledWithValue_expectedValueInterval
  , writeAttributeLocalConfigDisabledWithValue_expectedValueInterval_params
  , readAttributeReachableWithParams
  , readAttributeUniqueIDWithParams
  , readAttributeCapabilityMinimaWithParams
  , readAttributeProductAppearanceWithParams
  , readAttributeSpecificationVersionWithParams
  , readAttributeMaxPathsPerInvokeWithParams
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
  , newSelector
  , readAttributeAcceptedCommandListWithParamsSelector
  , readAttributeAttributeListWithParamsSelector
  , readAttributeCapabilityMinimaWithParamsSelector
  , readAttributeClusterRevisionWithParamsSelector
  , readAttributeConfigurationVersionWithParamsSelector
  , readAttributeDataModelRevisionWithParamsSelector
  , readAttributeFeatureMapWithParamsSelector
  , readAttributeGeneratedCommandListWithParamsSelector
  , readAttributeHardwareVersionStringWithParamsSelector
  , readAttributeHardwareVersionWithParamsSelector
  , readAttributeLocalConfigDisabledWithParamsSelector
  , readAttributeLocationWithParamsSelector
  , readAttributeManufacturingDateWithParamsSelector
  , readAttributeMaxPathsPerInvokeWithParamsSelector
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
  , readAttributeSpecificationVersionWithParamsSelector
  , readAttributeUniqueIDWithParamsSelector
  , readAttributeVendorIDWithParamsSelector
  , readAttributeVendorNameWithParamsSelector
  , writeAttributeLocalConfigDisabledWithValue_expectedValueIntervalSelector
  , writeAttributeLocalConfigDisabledWithValue_expectedValueInterval_paramsSelector
  , writeAttributeLocationWithValue_expectedValueIntervalSelector
  , writeAttributeLocationWithValue_expectedValueInterval_paramsSelector
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

-- | @- readAttributeDataModelRevisionWithParams:@
readAttributeDataModelRevisionWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeDataModelRevisionWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeDataModelRevisionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeVendorNameWithParams:@
readAttributeVendorNameWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeVendorNameWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeVendorNameWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeVendorIDWithParams:@
readAttributeVendorIDWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeVendorIDWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeVendorIDWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeProductNameWithParams:@
readAttributeProductNameWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeProductNameWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeProductNameWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeProductIDWithParams:@
readAttributeProductIDWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeProductIDWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeProductIDWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeNodeLabelWithParams:@
readAttributeNodeLabelWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeNodeLabelWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeNodeLabelWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeNodeLabelWithValue:expectedValueInterval:@
writeAttributeNodeLabelWithValue_expectedValueInterval :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBasicInformation -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeNodeLabelWithValue_expectedValueInterval mtrClusterBasicInformation dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBasicInformation writeAttributeNodeLabelWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeNodeLabelWithValue:expectedValueInterval:params:@
writeAttributeNodeLabelWithValue_expectedValueInterval_params :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBasicInformation -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeNodeLabelWithValue_expectedValueInterval_params mtrClusterBasicInformation dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBasicInformation writeAttributeNodeLabelWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeLocationWithParams:@
readAttributeLocationWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeLocationWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeLocationWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeLocationWithValue:expectedValueInterval:@
writeAttributeLocationWithValue_expectedValueInterval :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBasicInformation -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLocationWithValue_expectedValueInterval mtrClusterBasicInformation dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBasicInformation writeAttributeLocationWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeLocationWithValue:expectedValueInterval:params:@
writeAttributeLocationWithValue_expectedValueInterval_params :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBasicInformation -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLocationWithValue_expectedValueInterval_params mtrClusterBasicInformation dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBasicInformation writeAttributeLocationWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeHardwareVersionWithParams:@
readAttributeHardwareVersionWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeHardwareVersionWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeHardwareVersionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeHardwareVersionStringWithParams:@
readAttributeHardwareVersionStringWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeHardwareVersionStringWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeHardwareVersionStringWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSoftwareVersionWithParams:@
readAttributeSoftwareVersionWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeSoftwareVersionWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeSoftwareVersionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSoftwareVersionStringWithParams:@
readAttributeSoftwareVersionStringWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeSoftwareVersionStringWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeSoftwareVersionStringWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeManufacturingDateWithParams:@
readAttributeManufacturingDateWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeManufacturingDateWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeManufacturingDateWithParamsSelector (toMTRReadParams params)

-- | @- readAttributePartNumberWithParams:@
readAttributePartNumberWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributePartNumberWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributePartNumberWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeProductURLWithParams:@
readAttributeProductURLWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeProductURLWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeProductURLWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeProductLabelWithParams:@
readAttributeProductLabelWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeProductLabelWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeProductLabelWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSerialNumberWithParams:@
readAttributeSerialNumberWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeSerialNumberWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeSerialNumberWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeLocalConfigDisabledWithParams:@
readAttributeLocalConfigDisabledWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeLocalConfigDisabledWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeLocalConfigDisabledWithParamsSelector (toMTRReadParams params)

-- | @- writeAttributeLocalConfigDisabledWithValue:expectedValueInterval:@
writeAttributeLocalConfigDisabledWithValue_expectedValueInterval :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs) => mtrClusterBasicInformation -> dataValueDictionary -> expectedValueIntervalMs -> IO ()
writeAttributeLocalConfigDisabledWithValue_expectedValueInterval mtrClusterBasicInformation dataValueDictionary expectedValueIntervalMs =
  sendMessage mtrClusterBasicInformation writeAttributeLocalConfigDisabledWithValue_expectedValueIntervalSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs)

-- | @- writeAttributeLocalConfigDisabledWithValue:expectedValueInterval:params:@
writeAttributeLocalConfigDisabledWithValue_expectedValueInterval_params :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsNSDictionary dataValueDictionary, IsNSNumber expectedValueIntervalMs, IsMTRWriteParams params) => mtrClusterBasicInformation -> dataValueDictionary -> expectedValueIntervalMs -> params -> IO ()
writeAttributeLocalConfigDisabledWithValue_expectedValueInterval_params mtrClusterBasicInformation dataValueDictionary expectedValueIntervalMs params =
  sendMessage mtrClusterBasicInformation writeAttributeLocalConfigDisabledWithValue_expectedValueInterval_paramsSelector (toNSDictionary dataValueDictionary) (toNSNumber expectedValueIntervalMs) (toMTRWriteParams params)

-- | @- readAttributeReachableWithParams:@
readAttributeReachableWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeReachableWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeReachableWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeUniqueIDWithParams:@
readAttributeUniqueIDWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeUniqueIDWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeUniqueIDWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeCapabilityMinimaWithParams:@
readAttributeCapabilityMinimaWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeCapabilityMinimaWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeCapabilityMinimaWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeProductAppearanceWithParams:@
readAttributeProductAppearanceWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeProductAppearanceWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeProductAppearanceWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeSpecificationVersionWithParams:@
readAttributeSpecificationVersionWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeSpecificationVersionWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeSpecificationVersionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeMaxPathsPerInvokeWithParams:@
readAttributeMaxPathsPerInvokeWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeMaxPathsPerInvokeWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeMaxPathsPerInvokeWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeConfigurationVersionWithParams:@
readAttributeConfigurationVersionWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeConfigurationVersionWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeConfigurationVersionWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeGeneratedCommandListWithParams:@
readAttributeGeneratedCommandListWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeGeneratedCommandListWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeGeneratedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAcceptedCommandListWithParams:@
readAttributeAcceptedCommandListWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeAcceptedCommandListWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeAcceptedCommandListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeAttributeListWithParams:@
readAttributeAttributeListWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeAttributeListWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeAttributeListWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeFeatureMapWithParams:@
readAttributeFeatureMapWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeFeatureMapWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeFeatureMapWithParamsSelector (toMTRReadParams params)

-- | @- readAttributeClusterRevisionWithParams:@
readAttributeClusterRevisionWithParams :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRReadParams params) => mtrClusterBasicInformation -> params -> IO (Id NSDictionary)
readAttributeClusterRevisionWithParams mtrClusterBasicInformation params =
  sendMessage mtrClusterBasicInformation readAttributeClusterRevisionWithParamsSelector (toMTRReadParams params)

-- | @- init@
init_ :: IsMTRClusterBasicInformation mtrClusterBasicInformation => mtrClusterBasicInformation -> IO (Id MTRClusterBasicInformation)
init_ mtrClusterBasicInformation =
  sendOwnedMessage mtrClusterBasicInformation initSelector

-- | @+ new@
new :: IO (Id MTRClusterBasicInformation)
new  =
  do
    cls' <- getRequiredClass "MTRClusterBasicInformation"
    sendOwnedClassMessage cls' newSelector

-- | For all instance methods that take a completion (i.e. command invocations), the completion will be called on the provided queue.
--
-- ObjC selector: @- initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queue :: (IsMTRClusterBasicInformation mtrClusterBasicInformation, IsMTRDevice device, IsNSNumber endpointID, IsNSObject queue) => mtrClusterBasicInformation -> device -> endpointID -> queue -> IO (Id MTRClusterBasicInformation)
initWithDevice_endpointID_queue mtrClusterBasicInformation device endpointID queue =
  sendOwnedMessage mtrClusterBasicInformation initWithDevice_endpointID_queueSelector (toMTRDevice device) (toNSNumber endpointID) (toNSObject queue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @readAttributeDataModelRevisionWithParams:@
readAttributeDataModelRevisionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeDataModelRevisionWithParamsSelector = mkSelector "readAttributeDataModelRevisionWithParams:"

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

-- | @Selector@ for @readAttributeLocationWithParams:@
readAttributeLocationWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLocationWithParamsSelector = mkSelector "readAttributeLocationWithParams:"

-- | @Selector@ for @writeAttributeLocationWithValue:expectedValueInterval:@
writeAttributeLocationWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeLocationWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLocationWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLocationWithValue:expectedValueInterval:params:@
writeAttributeLocationWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeLocationWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLocationWithValue:expectedValueInterval:params:"

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

-- | @Selector@ for @readAttributeLocalConfigDisabledWithParams:@
readAttributeLocalConfigDisabledWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeLocalConfigDisabledWithParamsSelector = mkSelector "readAttributeLocalConfigDisabledWithParams:"

-- | @Selector@ for @writeAttributeLocalConfigDisabledWithValue:expectedValueInterval:@
writeAttributeLocalConfigDisabledWithValue_expectedValueIntervalSelector :: Selector '[Id NSDictionary, Id NSNumber] ()
writeAttributeLocalConfigDisabledWithValue_expectedValueIntervalSelector = mkSelector "writeAttributeLocalConfigDisabledWithValue:expectedValueInterval:"

-- | @Selector@ for @writeAttributeLocalConfigDisabledWithValue:expectedValueInterval:params:@
writeAttributeLocalConfigDisabledWithValue_expectedValueInterval_paramsSelector :: Selector '[Id NSDictionary, Id NSNumber, Id MTRWriteParams] ()
writeAttributeLocalConfigDisabledWithValue_expectedValueInterval_paramsSelector = mkSelector "writeAttributeLocalConfigDisabledWithValue:expectedValueInterval:params:"

-- | @Selector@ for @readAttributeReachableWithParams:@
readAttributeReachableWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeReachableWithParamsSelector = mkSelector "readAttributeReachableWithParams:"

-- | @Selector@ for @readAttributeUniqueIDWithParams:@
readAttributeUniqueIDWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeUniqueIDWithParamsSelector = mkSelector "readAttributeUniqueIDWithParams:"

-- | @Selector@ for @readAttributeCapabilityMinimaWithParams:@
readAttributeCapabilityMinimaWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeCapabilityMinimaWithParamsSelector = mkSelector "readAttributeCapabilityMinimaWithParams:"

-- | @Selector@ for @readAttributeProductAppearanceWithParams:@
readAttributeProductAppearanceWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeProductAppearanceWithParamsSelector = mkSelector "readAttributeProductAppearanceWithParams:"

-- | @Selector@ for @readAttributeSpecificationVersionWithParams:@
readAttributeSpecificationVersionWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeSpecificationVersionWithParamsSelector = mkSelector "readAttributeSpecificationVersionWithParams:"

-- | @Selector@ for @readAttributeMaxPathsPerInvokeWithParams:@
readAttributeMaxPathsPerInvokeWithParamsSelector :: Selector '[Id MTRReadParams] (Id NSDictionary)
readAttributeMaxPathsPerInvokeWithParamsSelector = mkSelector "readAttributeMaxPathsPerInvokeWithParams:"

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
initSelector :: Selector '[] (Id MTRClusterBasicInformation)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRClusterBasicInformation)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithDevice:endpointID:queue:@
initWithDevice_endpointID_queueSelector :: Selector '[Id MTRDevice, Id NSNumber, Id NSObject] (Id MTRClusterBasicInformation)
initWithDevice_endpointID_queueSelector = mkSelector "initWithDevice:endpointID:queue:"

