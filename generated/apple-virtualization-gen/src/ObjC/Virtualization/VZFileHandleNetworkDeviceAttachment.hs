{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Network device attachment sending raw network packets over a file handle.
--
-- The file handle attachment transmits the raw packets/frames between the virtual network interface and a file handle.    The data transmitted through this attachment is at the level of the data link layer.
--
-- The file handle must hold a connected datagram socket.
--
-- See: VZNetworkDeviceConfiguration
--
-- See: VZVirtioNetworkDeviceConfiguration
--
-- Generated bindings for @VZFileHandleNetworkDeviceAttachment@.
module ObjC.Virtualization.VZFileHandleNetworkDeviceAttachment
  ( VZFileHandleNetworkDeviceAttachment
  , IsVZFileHandleNetworkDeviceAttachment(..)
  , initWithFileHandle
  , fileHandle
  , maximumTransmissionUnit
  , setMaximumTransmissionUnit
  , fileHandleSelector
  , initWithFileHandleSelector
  , maximumTransmissionUnitSelector
  , setMaximumTransmissionUnitSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize the attachment with a file handle.
--
-- @fileHandle@ — File handle holding a connected datagram socket.
--
-- ObjC selector: @- initWithFileHandle:@
initWithFileHandle :: (IsVZFileHandleNetworkDeviceAttachment vzFileHandleNetworkDeviceAttachment, IsNSFileHandle fileHandle) => vzFileHandleNetworkDeviceAttachment -> fileHandle -> IO (Id VZFileHandleNetworkDeviceAttachment)
initWithFileHandle vzFileHandleNetworkDeviceAttachment fileHandle =
  sendOwnedMessage vzFileHandleNetworkDeviceAttachment initWithFileHandleSelector (toNSFileHandle fileHandle)

-- | The file handle associated with this attachment.
--
-- ObjC selector: @- fileHandle@
fileHandle :: IsVZFileHandleNetworkDeviceAttachment vzFileHandleNetworkDeviceAttachment => vzFileHandleNetworkDeviceAttachment -> IO (Id NSFileHandle)
fileHandle vzFileHandleNetworkDeviceAttachment =
  sendMessage vzFileHandleNetworkDeviceAttachment fileHandleSelector

-- | The maximum transmission unit (MTU) associated with this attachment.
--
-- The client side of the associated datagram socket must be properly configured with the appropriate values for    @SO_SNDBUF@, and @SO_RCVBUF@, which can be set using the @setsockopt@ system call. The value of @SO_RCVBUF@ is    expected to be at least double the value of @SO_SNDBUF@, and for optimal performance, the value of @SO_RCVBUF@    is recommended to be four times the value of @SO_SNDBUF@.
--
-- The default MTU is 1500.    The maximum MTU allowed is 65535, and the minimum MTU allowed is 1500. An invalid MTU value will result in an invalid    virtual machine configuration.
--
-- ObjC selector: @- maximumTransmissionUnit@
maximumTransmissionUnit :: IsVZFileHandleNetworkDeviceAttachment vzFileHandleNetworkDeviceAttachment => vzFileHandleNetworkDeviceAttachment -> IO CLong
maximumTransmissionUnit vzFileHandleNetworkDeviceAttachment =
  sendMessage vzFileHandleNetworkDeviceAttachment maximumTransmissionUnitSelector

-- | The maximum transmission unit (MTU) associated with this attachment.
--
-- The client side of the associated datagram socket must be properly configured with the appropriate values for    @SO_SNDBUF@, and @SO_RCVBUF@, which can be set using the @setsockopt@ system call. The value of @SO_RCVBUF@ is    expected to be at least double the value of @SO_SNDBUF@, and for optimal performance, the value of @SO_RCVBUF@    is recommended to be four times the value of @SO_SNDBUF@.
--
-- The default MTU is 1500.    The maximum MTU allowed is 65535, and the minimum MTU allowed is 1500. An invalid MTU value will result in an invalid    virtual machine configuration.
--
-- ObjC selector: @- setMaximumTransmissionUnit:@
setMaximumTransmissionUnit :: IsVZFileHandleNetworkDeviceAttachment vzFileHandleNetworkDeviceAttachment => vzFileHandleNetworkDeviceAttachment -> CLong -> IO ()
setMaximumTransmissionUnit vzFileHandleNetworkDeviceAttachment value =
  sendMessage vzFileHandleNetworkDeviceAttachment setMaximumTransmissionUnitSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFileHandle:@
initWithFileHandleSelector :: Selector '[Id NSFileHandle] (Id VZFileHandleNetworkDeviceAttachment)
initWithFileHandleSelector = mkSelector "initWithFileHandle:"

-- | @Selector@ for @fileHandle@
fileHandleSelector :: Selector '[] (Id NSFileHandle)
fileHandleSelector = mkSelector "fileHandle"

-- | @Selector@ for @maximumTransmissionUnit@
maximumTransmissionUnitSelector :: Selector '[] CLong
maximumTransmissionUnitSelector = mkSelector "maximumTransmissionUnit"

-- | @Selector@ for @setMaximumTransmissionUnit:@
setMaximumTransmissionUnitSelector :: Selector '[CLong] ()
setMaximumTransmissionUnitSelector = mkSelector "setMaximumTransmissionUnit:"

