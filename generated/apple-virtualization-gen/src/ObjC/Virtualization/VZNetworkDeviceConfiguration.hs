{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for a network adapter configuration.
--
-- VZNetworkDeviceConfiguration should not be instantiated directly.    One of its subclasses like VZVirtioNetworkDeviceConfiguration should be used instead.
--
-- See: VZVirtioNetworkDeviceConfiguration
--
-- Generated bindings for @VZNetworkDeviceConfiguration@.
module ObjC.Virtualization.VZNetworkDeviceConfiguration
  ( VZNetworkDeviceConfiguration
  , IsVZNetworkDeviceConfiguration(..)
  , new
  , init_
  , macAddress
  , setMACAddress
  , attachment
  , setAttachment
  , attachmentSelector
  , initSelector
  , macAddressSelector
  , newSelector
  , setAttachmentSelector
  , setMACAddressSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VZNetworkDeviceConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZNetworkDeviceConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZNetworkDeviceConfiguration vzNetworkDeviceConfiguration => vzNetworkDeviceConfiguration -> IO (Id VZNetworkDeviceConfiguration)
init_ vzNetworkDeviceConfiguration =
  sendOwnedMessage vzNetworkDeviceConfiguration initSelector

-- | The media access control address of the device. The default is a random, locally administered, unicast address.
--
-- ObjC selector: @- MACAddress@
macAddress :: IsVZNetworkDeviceConfiguration vzNetworkDeviceConfiguration => vzNetworkDeviceConfiguration -> IO (Id VZMACAddress)
macAddress vzNetworkDeviceConfiguration =
  sendMessage vzNetworkDeviceConfiguration macAddressSelector

-- | The media access control address of the device. The default is a random, locally administered, unicast address.
--
-- ObjC selector: @- setMACAddress:@
setMACAddress :: (IsVZNetworkDeviceConfiguration vzNetworkDeviceConfiguration, IsVZMACAddress value) => vzNetworkDeviceConfiguration -> value -> IO ()
setMACAddress vzNetworkDeviceConfiguration value =
  sendMessage vzNetworkDeviceConfiguration setMACAddressSelector (toVZMACAddress value)

-- | Network device attachment. Defines how the virtual device interfaces with the host system. The default is nil.
--
-- See: VZBridgedNetworkDeviceAttachment
--
-- See: VZFileHandleNetworkDeviceAttachment
--
-- See: VZNATNetworkDeviceAttachment
--
-- ObjC selector: @- attachment@
attachment :: IsVZNetworkDeviceConfiguration vzNetworkDeviceConfiguration => vzNetworkDeviceConfiguration -> IO (Id VZNetworkDeviceAttachment)
attachment vzNetworkDeviceConfiguration =
  sendMessage vzNetworkDeviceConfiguration attachmentSelector

-- | Network device attachment. Defines how the virtual device interfaces with the host system. The default is nil.
--
-- See: VZBridgedNetworkDeviceAttachment
--
-- See: VZFileHandleNetworkDeviceAttachment
--
-- See: VZNATNetworkDeviceAttachment
--
-- ObjC selector: @- setAttachment:@
setAttachment :: (IsVZNetworkDeviceConfiguration vzNetworkDeviceConfiguration, IsVZNetworkDeviceAttachment value) => vzNetworkDeviceConfiguration -> value -> IO ()
setAttachment vzNetworkDeviceConfiguration value =
  sendMessage vzNetworkDeviceConfiguration setAttachmentSelector (toVZNetworkDeviceAttachment value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZNetworkDeviceConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZNetworkDeviceConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @MACAddress@
macAddressSelector :: Selector '[] (Id VZMACAddress)
macAddressSelector = mkSelector "MACAddress"

-- | @Selector@ for @setMACAddress:@
setMACAddressSelector :: Selector '[Id VZMACAddress] ()
setMACAddressSelector = mkSelector "setMACAddress:"

-- | @Selector@ for @attachment@
attachmentSelector :: Selector '[] (Id VZNetworkDeviceAttachment)
attachmentSelector = mkSelector "attachment"

-- | @Selector@ for @setAttachment:@
setAttachmentSelector :: Selector '[Id VZNetworkDeviceAttachment] ()
setAttachmentSelector = mkSelector "setAttachment:"

