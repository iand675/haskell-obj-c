{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A means of discovering and monitoring connection / disconnection of external sync devices to the host.
--
-- ``AVExternalSyncDeviceDiscoverySession`` is a singleton that lists the external sync devices connected to the host. The client is expected to key-value observe the ``AVExternalSyncDeviceDiscoverySession/devices`` property for changes to the external sync devices list.
--
-- Generated bindings for @AVExternalSyncDeviceDiscoverySession@.
module ObjC.AVFoundation.AVExternalSyncDeviceDiscoverySession
  ( AVExternalSyncDeviceDiscoverySession
  , IsAVExternalSyncDeviceDiscoverySession(..)
  , init_
  , new
  , sharedSession
  , supported
  , devices
  , devicesSelector
  , initSelector
  , newSelector
  , sharedSessionSelector
  , supportedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVExternalSyncDeviceDiscoverySession avExternalSyncDeviceDiscoverySession => avExternalSyncDeviceDiscoverySession -> IO (Id AVExternalSyncDeviceDiscoverySession)
init_ avExternalSyncDeviceDiscoverySession =
  sendOwnedMessage avExternalSyncDeviceDiscoverySession initSelector

-- | @+ new@
new :: IO (Id AVExternalSyncDeviceDiscoverySession)
new  =
  do
    cls' <- getRequiredClass "AVExternalSyncDeviceDiscoverySession"
    sendOwnedClassMessage cls' newSelector

-- | The singleton instance of the external sync source device discovery session.
--
-- Access the one and only external sync device discovery session on this host device using this method. ``sharedSession`` returns @nil@ if the host device doesn't support external sync devices.
--
-- ObjC selector: @+ sharedSession@
sharedSession :: IO (Id AVExternalSyncDeviceDiscoverySession)
sharedSession  =
  do
    cls' <- getRequiredClass "AVExternalSyncDeviceDiscoverySession"
    sendClassMessage cls' sharedSessionSelector

-- | Whether external sync devices are supported by this device.
--
-- A value of @true@ indicates that external sync devices are supported while @false@ indicates they are not.
--
-- ObjC selector: @+ supported@
supported :: IO Bool
supported  =
  do
    cls' <- getRequiredClass "AVExternalSyncDeviceDiscoverySession"
    sendClassMessage cls' supportedSelector

-- | An array of external sync devices connected to this host.
--
-- The list is updated when external sync devices are connected to the host and they remain in the list until they become unavailable. This property is key-value observable.
--
-- ObjC selector: @- devices@
devices :: IsAVExternalSyncDeviceDiscoverySession avExternalSyncDeviceDiscoverySession => avExternalSyncDeviceDiscoverySession -> IO (Id NSArray)
devices avExternalSyncDeviceDiscoverySession =
  sendMessage avExternalSyncDeviceDiscoverySession devicesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVExternalSyncDeviceDiscoverySession)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVExternalSyncDeviceDiscoverySession)
newSelector = mkSelector "new"

-- | @Selector@ for @sharedSession@
sharedSessionSelector :: Selector '[] (Id AVExternalSyncDeviceDiscoverySession)
sharedSessionSelector = mkSelector "sharedSession"

-- | @Selector@ for @supported@
supportedSelector :: Selector '[] Bool
supportedSelector = mkSelector "supported"

-- | @Selector@ for @devices@
devicesSelector :: Selector '[] (Id NSArray)
devicesSelector = mkSelector "devices"

