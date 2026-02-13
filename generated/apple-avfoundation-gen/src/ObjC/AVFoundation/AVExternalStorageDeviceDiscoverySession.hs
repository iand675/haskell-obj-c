{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVExternalStorageDeviceDiscoverySession
--
-- AVExternalStorageDeviceDiscoverySession is used to monitor connection / disconnection of external storage devices to the device.
--
-- AVExternalStorageDeviceDiscoverySession is a singleton that lists the external storage devices connected to this device. The client is expected to key-value observe the externalStorageDevices property for changes to the external storage devices list.
--
-- Generated bindings for @AVExternalStorageDeviceDiscoverySession@.
module ObjC.AVFoundation.AVExternalStorageDeviceDiscoverySession
  ( AVExternalStorageDeviceDiscoverySession
  , IsAVExternalStorageDeviceDiscoverySession(..)
  , init_
  , new
  , sharedSession
  , externalStorageDevices
  , supported
  , externalStorageDevicesSelector
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
init_ :: IsAVExternalStorageDeviceDiscoverySession avExternalStorageDeviceDiscoverySession => avExternalStorageDeviceDiscoverySession -> IO (Id AVExternalStorageDeviceDiscoverySession)
init_ avExternalStorageDeviceDiscoverySession =
  sendOwnedMessage avExternalStorageDeviceDiscoverySession initSelector

-- | @+ new@
new :: IO (Id AVExternalStorageDeviceDiscoverySession)
new  =
  do
    cls' <- getRequiredClass "AVExternalStorageDeviceDiscoverySession"
    sendOwnedClassMessage cls' newSelector

-- | \@property sharedSession
--
-- Returns the singleton instance of the external storage device discovery session.
--
-- There is only one external storage device discovery session for each host device which can be accessed using this method. Will return nil if the device doesn't support external storage devices.
--
-- ObjC selector: @+ sharedSession@
sharedSession :: IO (Id AVExternalStorageDeviceDiscoverySession)
sharedSession  =
  do
    cls' <- getRequiredClass "AVExternalStorageDeviceDiscoverySession"
    sendClassMessage cls' sharedSessionSelector

-- | externalStorageDevices
--
-- An array of external storage devices connected to this device. Read only. Key-value observable.
--
-- An array of AVExternalStorageDevice objects connected to this device. The list is updated when the external storage device detected status changes.
--
-- ObjC selector: @- externalStorageDevices@
externalStorageDevices :: IsAVExternalStorageDeviceDiscoverySession avExternalStorageDeviceDiscoverySession => avExternalStorageDeviceDiscoverySession -> IO (Id NSArray)
externalStorageDevices avExternalStorageDeviceDiscoverySession =
  sendMessage avExternalStorageDeviceDiscoverySession externalStorageDevicesSelector

-- | supported
--
-- Whether the external storage devices are supported by this device.
--
-- A value of YES indicates that external storage devices are supported while NO indicates it is not.
--
-- ObjC selector: @+ supported@
supported :: IO Bool
supported  =
  do
    cls' <- getRequiredClass "AVExternalStorageDeviceDiscoverySession"
    sendClassMessage cls' supportedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVExternalStorageDeviceDiscoverySession)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVExternalStorageDeviceDiscoverySession)
newSelector = mkSelector "new"

-- | @Selector@ for @sharedSession@
sharedSessionSelector :: Selector '[] (Id AVExternalStorageDeviceDiscoverySession)
sharedSessionSelector = mkSelector "sharedSession"

-- | @Selector@ for @externalStorageDevices@
externalStorageDevicesSelector :: Selector '[] (Id NSArray)
externalStorageDevicesSelector = mkSelector "externalStorageDevices"

-- | @Selector@ for @supported@
supportedSelector :: Selector '[] Bool
supportedSelector = mkSelector "supported"

