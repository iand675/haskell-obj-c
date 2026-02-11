{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Storage device attachment backed by a Network Block Device (NBD) client.
--
-- This storage device attachment provides an NBD client implementation. The NBD client is connected    to an NBD server referred to by an NBD Uniform Resource Indicator (URI), represented as an URL in    this API. The NBD server runs outside of Virtualization framework and is not controlled by    Virtualization framework. The NBD client forwards the guest's I/O operations to the NBD server,    where the I/O operations are handled.
--
-- The NBD client will attempt to connect to the NBD server referred to by the URL when you start the virtual    machine (e.g. when @[VZVirtualMachine startWithCompletionHandler:]@ is called). A connection attempt is NOT    made when the attachment object is initialized. Reconnection attempts will take place throughout the life    cycle of the virtual machine when the NBD client encounters a recoverable error such as connection timeout    and unexpected connection errors. The NBD client will disconnect from the server when the virtual machine    shuts down.
--
-- Using this attachment requires the app to have the "com.apple.security.network.client" entitlement as this attachment opens an outgoing    network connection.
--
-- For more information about NBD, see https://github.com/NetworkBlockDevice/nbd/blob/master/doc/proto.md.    For more information about the NBD URL format, see https://github.com/NetworkBlockDevice/nbd/blob/master/doc/uri.md.
--
-- An example use of this API is:    ```    NSURL *url = [[NSURL alloc] initWithString:"nbd://localhost:10809/myDisk"]    NSError *error = nil;    VZNetworkBlockDeviceStorageDeviceAttachment *attachment =        [[VZNetworkBlockDeviceStorageDeviceAttachment alloc] initWithURL:url                                                                 timeout:5.0                                                          forcedReadOnly:NO                                                     synchronizationMode:VZDiskSynchronizationModeFull                                                                   error:&error];
--
-- if (!attachment) {        // Handle the @error@.    }
--
-- VZVirtioBlockDeviceConfiguration *blockDevice = [[VZVirtioBlockDeviceConfiguration alloc] initWithAttachment:attachment];    ```
--
-- Generated bindings for @VZNetworkBlockDeviceStorageDeviceAttachment@.
module ObjC.Virtualization.VZNetworkBlockDeviceStorageDeviceAttachment
  ( VZNetworkBlockDeviceStorageDeviceAttachment
  , IsVZNetworkBlockDeviceStorageDeviceAttachment(..)
  , initWithURL_timeout_forcedReadOnly_synchronizationMode_error
  , initWithURL_error
  , validateURL_error
  , url
  , timeout
  , forcedReadOnly
  , synchronizationMode
  , delegate
  , setDelegate
  , initWithURL_timeout_forcedReadOnly_synchronizationMode_errorSelector
  , initWithURL_errorSelector
  , validateURL_errorSelector
  , urlSelector
  , timeoutSelector
  , forcedReadOnlySelector
  , synchronizationModeSelector
  , delegateSelector
  , setDelegateSelector

  -- * Enum types
  , VZDiskSynchronizationMode(VZDiskSynchronizationMode)
  , pattern VZDiskSynchronizationModeFull
  , pattern VZDiskSynchronizationModeNone

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
import ObjC.Virtualization.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initialize the attachment from an NBD Uniform Resource Indicator (URI) represented as an URL.
--
-- @URL@ — The URL referring to the NBD server to which the NBD client is to be connected.
--
-- @timeout@ — The timeout value in seconds for the connection between the client and server. When the timeout expires, an attempt to reconnect with the server will take place.
--
-- @forcedReadOnly@ — If YES, the disk attachment is forced to be read-only, regardless of whether or not the NBD server supports write requests.
--
-- @error@ — If not nil, assigned with the error if the initialization failed.
--
-- Returns: An initialized @VZNetworkBlockDeviceStorageDeviceAttachment@ or nil if there was an error.
--
-- The @forcedReadOnly@ parameter affects how the NBD client is exposed to the guest operating system    by the storage controller. As part of the NBD protocol, whether or not the disk exposed by the NBD client is    read-only is advertised by the NBD server during the handshake phase of the protocol. Setting @forcedReadOnly@    to YES will force the NBD client to show up as read-only to the guest regardless of whether or not the NBD    server advertises itself as read-only.
--
-- ObjC selector: @- initWithURL:timeout:forcedReadOnly:synchronizationMode:error:@
initWithURL_timeout_forcedReadOnly_synchronizationMode_error :: (IsVZNetworkBlockDeviceStorageDeviceAttachment vzNetworkBlockDeviceStorageDeviceAttachment, IsNSURL url, IsNSError error_) => vzNetworkBlockDeviceStorageDeviceAttachment -> url -> CDouble -> Bool -> VZDiskSynchronizationMode -> error_ -> IO (Id VZNetworkBlockDeviceStorageDeviceAttachment)
initWithURL_timeout_forcedReadOnly_synchronizationMode_error vzNetworkBlockDeviceStorageDeviceAttachment  url timeout forcedReadOnly synchronizationMode error_ =
  withObjCPtr url $ \raw_url ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg vzNetworkBlockDeviceStorageDeviceAttachment (mkSelector "initWithURL:timeout:forcedReadOnly:synchronizationMode:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCDouble timeout, argCULong (if forcedReadOnly then 1 else 0), argCLong (coerce synchronizationMode), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | Convenience initializer to create the attachment from an NBD URL.
--
-- @URL@ — The URL referring to the NBD server to which the NBD client is to be connected.
--
-- @error@ — If not nil, assigned with the error if the initialization failed.
--
-- Returns: An initialized @VZNetworkBlockDeviceStorageDeviceAttachment@ or nil if there was an error.
--
-- This initializer automatically assigns optimized default values for the @timeout@,    @forcedReadOnly@, and @synchronizationMode@ properties.
--
-- ObjC selector: @- initWithURL:error:@
initWithURL_error :: (IsVZNetworkBlockDeviceStorageDeviceAttachment vzNetworkBlockDeviceStorageDeviceAttachment, IsNSURL url, IsNSError error_) => vzNetworkBlockDeviceStorageDeviceAttachment -> url -> error_ -> IO (Id VZNetworkBlockDeviceStorageDeviceAttachment)
initWithURL_error vzNetworkBlockDeviceStorageDeviceAttachment  url error_ =
  withObjCPtr url $ \raw_url ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg vzNetworkBlockDeviceStorageDeviceAttachment (mkSelector "initWithURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | Check if URL is a valid NBD URL.
--
-- @URL@ — The NBD URL to validate.
--
-- @error@ — If not nil, assigned with an error describing why the URL is not valid.
--
-- See https://github.com/NetworkBlockDevice/nbd/blob/master/doc/uri.md for more detailed descriptions    of valid URIs.
--
-- This method checks that the URL is well-formed, it does not attempt to access the URL.
--
-- ObjC selector: @+ validateURL:error:@
validateURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO Bool
validateURL_error url error_ =
  do
    cls' <- getRequiredClass "VZNetworkBlockDeviceStorageDeviceAttachment"
    withObjCPtr url $ \raw_url ->
      withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "validateURL:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | URL referring to the NBD server to which the NBD client is to be connected.
--
-- ObjC selector: @- URL@
url :: IsVZNetworkBlockDeviceStorageDeviceAttachment vzNetworkBlockDeviceStorageDeviceAttachment => vzNetworkBlockDeviceStorageDeviceAttachment -> IO (Id NSURL)
url vzNetworkBlockDeviceStorageDeviceAttachment  =
    sendMsg vzNetworkBlockDeviceStorageDeviceAttachment (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The timeout value in seconds for the connection between the client and server. When the timeout expires, an attempt to reconnect with the server will take place.
--
-- ObjC selector: @- timeout@
timeout :: IsVZNetworkBlockDeviceStorageDeviceAttachment vzNetworkBlockDeviceStorageDeviceAttachment => vzNetworkBlockDeviceStorageDeviceAttachment -> IO CDouble
timeout vzNetworkBlockDeviceStorageDeviceAttachment  =
    sendMsg vzNetworkBlockDeviceStorageDeviceAttachment (mkSelector "timeout") retCDouble []

-- | Whether the underlying disk attachment is forced to be read-only.
--
-- The @forcedReadOnly@ parameter affects how the NBD client is exposed to the guest operating system    by the storage controller. As part of the NBD protocol, whether or not the disk exposed by the NBD client    is read-only is advertised by the NBD server during the handshake phase of the protocol. Setting    @forcedReadOnly@ to YES will force the NBD client to show up as read-only to the    guest regardless of whether or not the NBD server advertises itself as read-only.
--
-- ObjC selector: @- forcedReadOnly@
forcedReadOnly :: IsVZNetworkBlockDeviceStorageDeviceAttachment vzNetworkBlockDeviceStorageDeviceAttachment => vzNetworkBlockDeviceStorageDeviceAttachment -> IO Bool
forcedReadOnly vzNetworkBlockDeviceStorageDeviceAttachment  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzNetworkBlockDeviceStorageDeviceAttachment (mkSelector "forcedReadOnly") retCULong []

-- | The mode in which the NBD client synchronizes data with the NBD server.
--
-- ObjC selector: @- synchronizationMode@
synchronizationMode :: IsVZNetworkBlockDeviceStorageDeviceAttachment vzNetworkBlockDeviceStorageDeviceAttachment => vzNetworkBlockDeviceStorageDeviceAttachment -> IO VZDiskSynchronizationMode
synchronizationMode vzNetworkBlockDeviceStorageDeviceAttachment  =
    fmap (coerce :: CLong -> VZDiskSynchronizationMode) $ sendMsg vzNetworkBlockDeviceStorageDeviceAttachment (mkSelector "synchronizationMode") retCLong []

-- | The attachment's delegate.
--
-- ObjC selector: @- delegate@
delegate :: IsVZNetworkBlockDeviceStorageDeviceAttachment vzNetworkBlockDeviceStorageDeviceAttachment => vzNetworkBlockDeviceStorageDeviceAttachment -> IO RawId
delegate vzNetworkBlockDeviceStorageDeviceAttachment  =
    fmap (RawId . castPtr) $ sendMsg vzNetworkBlockDeviceStorageDeviceAttachment (mkSelector "delegate") (retPtr retVoid) []

-- | The attachment's delegate.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsVZNetworkBlockDeviceStorageDeviceAttachment vzNetworkBlockDeviceStorageDeviceAttachment => vzNetworkBlockDeviceStorageDeviceAttachment -> RawId -> IO ()
setDelegate vzNetworkBlockDeviceStorageDeviceAttachment  value =
    sendMsg vzNetworkBlockDeviceStorageDeviceAttachment (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:timeout:forcedReadOnly:synchronizationMode:error:@
initWithURL_timeout_forcedReadOnly_synchronizationMode_errorSelector :: Selector
initWithURL_timeout_forcedReadOnly_synchronizationMode_errorSelector = mkSelector "initWithURL:timeout:forcedReadOnly:synchronizationMode:error:"

-- | @Selector@ for @initWithURL:error:@
initWithURL_errorSelector :: Selector
initWithURL_errorSelector = mkSelector "initWithURL:error:"

-- | @Selector@ for @validateURL:error:@
validateURL_errorSelector :: Selector
validateURL_errorSelector = mkSelector "validateURL:error:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @timeout@
timeoutSelector :: Selector
timeoutSelector = mkSelector "timeout"

-- | @Selector@ for @forcedReadOnly@
forcedReadOnlySelector :: Selector
forcedReadOnlySelector = mkSelector "forcedReadOnly"

-- | @Selector@ for @synchronizationMode@
synchronizationModeSelector :: Selector
synchronizationModeSelector = mkSelector "synchronizationMode"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

