{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Device-related event (e.g. found, lost).
--
-- Generated bindings for @DDDeviceEvent@.
module ObjC.DeviceDiscoveryExtension.DDDeviceEvent
  ( DDDeviceEvent
  , IsDDDeviceEvent(..)
  , initWithEventType_device
  , device
  , eventType
  , deviceSelector
  , eventTypeSelector
  , initWithEventType_deviceSelector

  -- * Enum types
  , DDEventType(DDEventType)
  , pattern DDEventTypeUnknown
  , pattern DDEventTypeDeviceFound
  , pattern DDEventTypeDeviceLost
  , pattern DDEventTypeDeviceChanged

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.DeviceDiscoveryExtension.Internal.Classes
import ObjC.DeviceDiscoveryExtension.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initializes a device event.
--
-- ObjC selector: @- initWithEventType:device:@
initWithEventType_device :: (IsDDDeviceEvent ddDeviceEvent, IsDDDevice device) => ddDeviceEvent -> DDEventType -> device -> IO (Id DDDeviceEvent)
initWithEventType_device ddDeviceEvent type_ device =
  sendOwnedMessage ddDeviceEvent initWithEventType_deviceSelector type_ (toDDDevice device)

-- | Device found or lost.
--
-- ObjC selector: @- device@
device :: IsDDDeviceEvent ddDeviceEvent => ddDeviceEvent -> IO (Id DDDevice)
device ddDeviceEvent =
  sendMessage ddDeviceEvent deviceSelector

-- | Type of event.
--
-- ObjC selector: @- eventType@
eventType :: IsDDDeviceEvent ddDeviceEvent => ddDeviceEvent -> IO DDEventType
eventType ddDeviceEvent =
  sendMessage ddDeviceEvent eventTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithEventType:device:@
initWithEventType_deviceSelector :: Selector '[DDEventType, Id DDDevice] (Id DDDeviceEvent)
initWithEventType_deviceSelector = mkSelector "initWithEventType:device:"

-- | @Selector@ for @device@
deviceSelector :: Selector '[] (Id DDDevice)
deviceSelector = mkSelector "device"

-- | @Selector@ for @eventType@
eventTypeSelector :: Selector '[] DDEventType
eventTypeSelector = mkSelector "eventType"

