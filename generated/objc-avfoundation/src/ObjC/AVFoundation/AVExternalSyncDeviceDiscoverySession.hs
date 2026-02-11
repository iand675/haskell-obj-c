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
  , initSelector
  , newSelector
  , sharedSessionSelector
  , supportedSelector
  , devicesSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVExternalSyncDeviceDiscoverySession avExternalSyncDeviceDiscoverySession => avExternalSyncDeviceDiscoverySession -> IO (Id AVExternalSyncDeviceDiscoverySession)
init_ avExternalSyncDeviceDiscoverySession  =
  sendMsg avExternalSyncDeviceDiscoverySession (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVExternalSyncDeviceDiscoverySession)
new  =
  do
    cls' <- getRequiredClass "AVExternalSyncDeviceDiscoverySession"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The singleton instance of the external sync source device discovery session.
--
-- Access the one and only external sync device discovery session on this host device using this method. ``sharedSession`` returns @nil@ if the host device doesn't support external sync devices.
--
-- ObjC selector: @+ sharedSession@
sharedSession :: IO (Id AVExternalSyncDeviceDiscoverySession)
sharedSession  =
  do
    cls' <- getRequiredClass "AVExternalSyncDeviceDiscoverySession"
    sendClassMsg cls' (mkSelector "sharedSession") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Whether external sync devices are supported by this device.
--
-- A value of @true@ indicates that external sync devices are supported while @false@ indicates they are not.
--
-- ObjC selector: @+ supported@
supported :: IO Bool
supported  =
  do
    cls' <- getRequiredClass "AVExternalSyncDeviceDiscoverySession"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "supported") retCULong []

-- | An array of external sync devices connected to this host.
--
-- The list is updated when external sync devices are connected to the host and they remain in the list until they become unavailable. This property is key-value observable.
--
-- ObjC selector: @- devices@
devices :: IsAVExternalSyncDeviceDiscoverySession avExternalSyncDeviceDiscoverySession => avExternalSyncDeviceDiscoverySession -> IO (Id NSArray)
devices avExternalSyncDeviceDiscoverySession  =
  sendMsg avExternalSyncDeviceDiscoverySession (mkSelector "devices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @sharedSession@
sharedSessionSelector :: Selector
sharedSessionSelector = mkSelector "sharedSession"

-- | @Selector@ for @supported@
supportedSelector :: Selector
supportedSelector = mkSelector "supported"

-- | @Selector@ for @devices@
devicesSelector :: Selector
devicesSelector = mkSelector "devices"

