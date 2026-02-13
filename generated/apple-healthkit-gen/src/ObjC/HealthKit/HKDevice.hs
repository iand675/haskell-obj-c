{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKDevice@.
module ObjC.HealthKit.HKDevice
  ( HKDevice
  , IsHKDevice(..)
  , initWithName_manufacturer_model_hardwareVersion_firmwareVersion_softwareVersion_localIdentifier_UDIDeviceIdentifier
  , init_
  , localDevice
  , name
  , manufacturer
  , model
  , hardwareVersion
  , firmwareVersion
  , softwareVersion
  , localIdentifier
  , udiDeviceIdentifier
  , firmwareVersionSelector
  , hardwareVersionSelector
  , initSelector
  , initWithName_manufacturer_model_hardwareVersion_firmwareVersion_softwareVersion_localIdentifier_UDIDeviceIdentifierSelector
  , localDeviceSelector
  , localIdentifierSelector
  , manufacturerSelector
  , modelSelector
  , nameSelector
  , softwareVersionSelector
  , udiDeviceIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithName:manufacturer:model:hardwareVersion:firmwareVersion:softwareVersion:localIdentifier:UDIDeviceIdentifier:
--
-- Initialize a new HKDevice with the specified values.
--
-- This allows initialization of an HKDevice object based on the                information provided.
--
-- ObjC selector: @- initWithName:manufacturer:model:hardwareVersion:firmwareVersion:softwareVersion:localIdentifier:UDIDeviceIdentifier:@
initWithName_manufacturer_model_hardwareVersion_firmwareVersion_softwareVersion_localIdentifier_UDIDeviceIdentifier :: (IsHKDevice hkDevice, IsNSString name, IsNSString manufacturer, IsNSString model, IsNSString hardwareVersion, IsNSString firmwareVersion, IsNSString softwareVersion, IsNSString localIdentifier, IsNSString udiDeviceIdentifier) => hkDevice -> name -> manufacturer -> model -> hardwareVersion -> firmwareVersion -> softwareVersion -> localIdentifier -> udiDeviceIdentifier -> IO (Id HKDevice)
initWithName_manufacturer_model_hardwareVersion_firmwareVersion_softwareVersion_localIdentifier_UDIDeviceIdentifier hkDevice name manufacturer model hardwareVersion firmwareVersion softwareVersion localIdentifier udiDeviceIdentifier =
  sendOwnedMessage hkDevice initWithName_manufacturer_model_hardwareVersion_firmwareVersion_softwareVersion_localIdentifier_UDIDeviceIdentifierSelector (toNSString name) (toNSString manufacturer) (toNSString model) (toNSString hardwareVersion) (toNSString firmwareVersion) (toNSString softwareVersion) (toNSString localIdentifier) (toNSString udiDeviceIdentifier)

-- | @- init@
init_ :: IsHKDevice hkDevice => hkDevice -> IO (Id HKDevice)
init_ hkDevice =
  sendOwnedMessage hkDevice initSelector

-- | localDevice
--
-- Returns a device representing the host.
--
-- If an app chooses to save samples that were retrieved from the local device, e.g. an HKWorkout with a                 totalDistance HKQuantity gathered from CoreLocation GPS distances, then this would be an appropriate                 HKDevice to use.
--
-- ObjC selector: @+ localDevice@
localDevice :: IO (Id HKDevice)
localDevice  =
  do
    cls' <- getRequiredClass "HKDevice"
    sendClassMessage cls' localDeviceSelector

-- | name
--
-- The name of the receiver.
--
-- The user-facing name, such as the one displayed in the Bluetooth Settings for a BLE device.
--
-- ObjC selector: @- name@
name :: IsHKDevice hkDevice => hkDevice -> IO (Id NSString)
name hkDevice =
  sendMessage hkDevice nameSelector

-- | manufacturer
--
-- The manufacturer of the receiver.
--
-- ObjC selector: @- manufacturer@
manufacturer :: IsHKDevice hkDevice => hkDevice -> IO (Id NSString)
manufacturer hkDevice =
  sendMessage hkDevice manufacturerSelector

-- | model
--
-- The model of the receiver.
--
-- ObjC selector: @- model@
model :: IsHKDevice hkDevice => hkDevice -> IO (Id NSString)
model hkDevice =
  sendMessage hkDevice modelSelector

-- | hardwareVersion
--
-- The hardware revision of the receiver.
--
-- ObjC selector: @- hardwareVersion@
hardwareVersion :: IsHKDevice hkDevice => hkDevice -> IO (Id NSString)
hardwareVersion hkDevice =
  sendMessage hkDevice hardwareVersionSelector

-- | firmwareVersion
--
-- The firmware revision of the receiver.
--
-- ObjC selector: @- firmwareVersion@
firmwareVersion :: IsHKDevice hkDevice => hkDevice -> IO (Id NSString)
firmwareVersion hkDevice =
  sendMessage hkDevice firmwareVersionSelector

-- | softwareVersion
--
-- The software revision of the receiver.
--
-- ObjC selector: @- softwareVersion@
softwareVersion :: IsHKDevice hkDevice => hkDevice -> IO (Id NSString)
softwareVersion hkDevice =
  sendMessage hkDevice softwareVersionSelector

-- | localIdentifier
--
-- A unique identifier for the receiver.
--
-- This property is available to clients for a local identifier.                For example, Bluetooth peripherals managed by HealthKit use this                for the CoreBluetooth UUID which is valid only on the local                device and thus distinguish the same Bluetooth peripheral used                between multiple devices.
--
-- ObjC selector: @- localIdentifier@
localIdentifier :: IsHKDevice hkDevice => hkDevice -> IO (Id NSString)
localIdentifier hkDevice =
  sendMessage hkDevice localIdentifierSelector

-- | UDIDeviceIdentifier
--
-- Represents the device identifier portion of a device's FDA UDI (Unique Device Identifier).
--
-- The device identifier can be used to reference the FDA's GUDID (Globally Unique Device                Identifier Database). Note that for user privacy concerns this field should not be used to                persist the production identifier portion of the device UDI. HealthKit clients should manage                the production identifier independently, if needed.                See http://www.fda.gov/MedicalDevices/DeviceRegulationandGuidance/UniqueDeviceIdentification/ for more information.
--
-- ObjC selector: @- UDIDeviceIdentifier@
udiDeviceIdentifier :: IsHKDevice hkDevice => hkDevice -> IO (Id NSString)
udiDeviceIdentifier hkDevice =
  sendMessage hkDevice udiDeviceIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:manufacturer:model:hardwareVersion:firmwareVersion:softwareVersion:localIdentifier:UDIDeviceIdentifier:@
initWithName_manufacturer_model_hardwareVersion_firmwareVersion_softwareVersion_localIdentifier_UDIDeviceIdentifierSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSString, Id NSString, Id NSString, Id NSString, Id NSString] (Id HKDevice)
initWithName_manufacturer_model_hardwareVersion_firmwareVersion_softwareVersion_localIdentifier_UDIDeviceIdentifierSelector = mkSelector "initWithName:manufacturer:model:hardwareVersion:firmwareVersion:softwareVersion:localIdentifier:UDIDeviceIdentifier:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKDevice)
initSelector = mkSelector "init"

-- | @Selector@ for @localDevice@
localDeviceSelector :: Selector '[] (Id HKDevice)
localDeviceSelector = mkSelector "localDevice"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @manufacturer@
manufacturerSelector :: Selector '[] (Id NSString)
manufacturerSelector = mkSelector "manufacturer"

-- | @Selector@ for @model@
modelSelector :: Selector '[] (Id NSString)
modelSelector = mkSelector "model"

-- | @Selector@ for @hardwareVersion@
hardwareVersionSelector :: Selector '[] (Id NSString)
hardwareVersionSelector = mkSelector "hardwareVersion"

-- | @Selector@ for @firmwareVersion@
firmwareVersionSelector :: Selector '[] (Id NSString)
firmwareVersionSelector = mkSelector "firmwareVersion"

-- | @Selector@ for @softwareVersion@
softwareVersionSelector :: Selector '[] (Id NSString)
softwareVersionSelector = mkSelector "softwareVersion"

-- | @Selector@ for @localIdentifier@
localIdentifierSelector :: Selector '[] (Id NSString)
localIdentifierSelector = mkSelector "localIdentifier"

-- | @Selector@ for @UDIDeviceIdentifier@
udiDeviceIdentifierSelector :: Selector '[] (Id NSString)
udiDeviceIdentifierSelector = mkSelector "UDIDeviceIdentifier"

