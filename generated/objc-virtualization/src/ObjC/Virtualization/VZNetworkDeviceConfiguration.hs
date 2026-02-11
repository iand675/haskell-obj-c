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
  , newSelector
  , initSelector
  , macAddressSelector
  , setMACAddressSelector
  , attachmentSelector
  , setAttachmentSelector


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

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VZNetworkDeviceConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZNetworkDeviceConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZNetworkDeviceConfiguration vzNetworkDeviceConfiguration => vzNetworkDeviceConfiguration -> IO (Id VZNetworkDeviceConfiguration)
init_ vzNetworkDeviceConfiguration  =
  sendMsg vzNetworkDeviceConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The media access control address of the device. The default is a random, locally administered, unicast address.
--
-- ObjC selector: @- MACAddress@
macAddress :: IsVZNetworkDeviceConfiguration vzNetworkDeviceConfiguration => vzNetworkDeviceConfiguration -> IO (Id VZMACAddress)
macAddress vzNetworkDeviceConfiguration  =
  sendMsg vzNetworkDeviceConfiguration (mkSelector "MACAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The media access control address of the device. The default is a random, locally administered, unicast address.
--
-- ObjC selector: @- setMACAddress:@
setMACAddress :: (IsVZNetworkDeviceConfiguration vzNetworkDeviceConfiguration, IsVZMACAddress value) => vzNetworkDeviceConfiguration -> value -> IO ()
setMACAddress vzNetworkDeviceConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzNetworkDeviceConfiguration (mkSelector "setMACAddress:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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
attachment vzNetworkDeviceConfiguration  =
  sendMsg vzNetworkDeviceConfiguration (mkSelector "attachment") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setAttachment vzNetworkDeviceConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzNetworkDeviceConfiguration (mkSelector "setAttachment:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @MACAddress@
macAddressSelector :: Selector
macAddressSelector = mkSelector "MACAddress"

-- | @Selector@ for @setMACAddress:@
setMACAddressSelector :: Selector
setMACAddressSelector = mkSelector "setMACAddress:"

-- | @Selector@ for @attachment@
attachmentSelector :: Selector
attachmentSelector = mkSelector "attachment"

-- | @Selector@ for @setAttachment:@
setAttachmentSelector :: Selector
setAttachmentSelector = mkSelector "setAttachment:"

