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
  , initWithFileHandleSelector
  , fileHandleSelector
  , maximumTransmissionUnitSelector
  , setMaximumTransmissionUnitSelector


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

-- | Initialize the attachment with a file handle.
--
-- @fileHandle@ — File handle holding a connected datagram socket.
--
-- ObjC selector: @- initWithFileHandle:@
initWithFileHandle :: (IsVZFileHandleNetworkDeviceAttachment vzFileHandleNetworkDeviceAttachment, IsNSFileHandle fileHandle) => vzFileHandleNetworkDeviceAttachment -> fileHandle -> IO (Id VZFileHandleNetworkDeviceAttachment)
initWithFileHandle vzFileHandleNetworkDeviceAttachment  fileHandle =
withObjCPtr fileHandle $ \raw_fileHandle ->
    sendMsg vzFileHandleNetworkDeviceAttachment (mkSelector "initWithFileHandle:") (retPtr retVoid) [argPtr (castPtr raw_fileHandle :: Ptr ())] >>= ownedObject . castPtr

-- | The file handle associated with this attachment.
--
-- ObjC selector: @- fileHandle@
fileHandle :: IsVZFileHandleNetworkDeviceAttachment vzFileHandleNetworkDeviceAttachment => vzFileHandleNetworkDeviceAttachment -> IO (Id NSFileHandle)
fileHandle vzFileHandleNetworkDeviceAttachment  =
  sendMsg vzFileHandleNetworkDeviceAttachment (mkSelector "fileHandle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The maximum transmission unit (MTU) associated with this attachment.
--
-- The client side of the associated datagram socket must be properly configured with the appropriate values for    @SO_SNDBUF@, and @SO_RCVBUF@, which can be set using the @setsockopt@ system call. The value of @SO_RCVBUF@ is    expected to be at least double the value of @SO_SNDBUF@, and for optimal performance, the value of @SO_RCVBUF@    is recommended to be four times the value of @SO_SNDBUF@.
--
-- The default MTU is 1500.    The maximum MTU allowed is 65535, and the minimum MTU allowed is 1500. An invalid MTU value will result in an invalid    virtual machine configuration.
--
-- ObjC selector: @- maximumTransmissionUnit@
maximumTransmissionUnit :: IsVZFileHandleNetworkDeviceAttachment vzFileHandleNetworkDeviceAttachment => vzFileHandleNetworkDeviceAttachment -> IO CLong
maximumTransmissionUnit vzFileHandleNetworkDeviceAttachment  =
  sendMsg vzFileHandleNetworkDeviceAttachment (mkSelector "maximumTransmissionUnit") retCLong []

-- | The maximum transmission unit (MTU) associated with this attachment.
--
-- The client side of the associated datagram socket must be properly configured with the appropriate values for    @SO_SNDBUF@, and @SO_RCVBUF@, which can be set using the @setsockopt@ system call. The value of @SO_RCVBUF@ is    expected to be at least double the value of @SO_SNDBUF@, and for optimal performance, the value of @SO_RCVBUF@    is recommended to be four times the value of @SO_SNDBUF@.
--
-- The default MTU is 1500.    The maximum MTU allowed is 65535, and the minimum MTU allowed is 1500. An invalid MTU value will result in an invalid    virtual machine configuration.
--
-- ObjC selector: @- setMaximumTransmissionUnit:@
setMaximumTransmissionUnit :: IsVZFileHandleNetworkDeviceAttachment vzFileHandleNetworkDeviceAttachment => vzFileHandleNetworkDeviceAttachment -> CLong -> IO ()
setMaximumTransmissionUnit vzFileHandleNetworkDeviceAttachment  value =
  sendMsg vzFileHandleNetworkDeviceAttachment (mkSelector "setMaximumTransmissionUnit:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFileHandle:@
initWithFileHandleSelector :: Selector
initWithFileHandleSelector = mkSelector "initWithFileHandle:"

-- | @Selector@ for @fileHandle@
fileHandleSelector :: Selector
fileHandleSelector = mkSelector "fileHandle"

-- | @Selector@ for @maximumTransmissionUnit@
maximumTransmissionUnitSelector :: Selector
maximumTransmissionUnitSelector = mkSelector "maximumTransmissionUnit"

-- | @Selector@ for @setMaximumTransmissionUnit:@
setMaximumTransmissionUnitSelector :: Selector
setMaximumTransmissionUnitSelector = mkSelector "setMaximumTransmissionUnit:"

