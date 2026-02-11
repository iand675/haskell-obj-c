{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NWTCPConnection
--
-- Establish TCP connections to an endpoint, and send and receive data on the TCP connection.
--
-- Generated bindings for @NWTCPConnection@.
module ObjC.NetworkExtension.NWTCPConnection
  ( NWTCPConnection
  , IsNWTCPConnection(..)
  , initWithUpgradeForConnection
  , cancel
  , readLength_completionHandler
  , readMinimumLength_maximumLength_completionHandler
  , write_completionHandler
  , writeClose
  , state
  , viable
  , hasBetterPath
  , endpoint
  , connectedPath
  , localAddress
  , remoteAddress
  , txtRecord
  , error_
  , initWithUpgradeForConnectionSelector
  , cancelSelector
  , readLength_completionHandlerSelector
  , readMinimumLength_maximumLength_completionHandlerSelector
  , write_completionHandlerSelector
  , writeCloseSelector
  , stateSelector
  , viableSelector
  , hasBetterPathSelector
  , endpointSelector
  , connectedPathSelector
  , localAddressSelector
  , remoteAddressSelector
  , txtRecordSelector
  , errorSelector

  -- * Enum types
  , NWTCPConnectionState(NWTCPConnectionState)
  , pattern NWTCPConnectionStateInvalid
  , pattern NWTCPConnectionStateConnecting
  , pattern NWTCPConnectionStateWaiting
  , pattern NWTCPConnectionStateConnected
  , pattern NWTCPConnectionStateDisconnected
  , pattern NWTCPConnectionStateCancelled

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

import ObjC.NetworkExtension.Internal.Classes
import ObjC.NetworkExtension.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | initWithUpgradeForConnection:
--
-- This convenience initializer can be used to create a new connection that would only 		be connected if there exists a better path (as determined by the system) to the destination 		endpoint of the original connection. It will be initialized using the same destination endpoint 		and set of parameters from the original connection.
--
-- If the original connection becomes disconnected or cancelled, the new "upgrade" connection 		would automatically be considered better.
--
-- The caller should create an NWTCPConnection and watch for the hasBetterPath property.		When this property is YES, the caller should attempt to create a new upgrade		connection, with the goal to start transferring data on the new better path as soon as		possible to reduce power and potentially monetary cost. When the new upgrade connection		becomes connected and when the caller wraps up the previous caller session on		the original connection, the caller can start using the new upgrade connection and		tear down the original one.
--
-- @connection@ — The original connection from which the caller will upgrade
--
-- Returns: An initialized NWTCPConnection
--
-- ObjC selector: @- initWithUpgradeForConnection:@
initWithUpgradeForConnection :: (IsNWTCPConnection nwtcpConnection, IsNWTCPConnection connection) => nwtcpConnection -> connection -> IO (Id NWTCPConnection)
initWithUpgradeForConnection nwtcpConnection  connection =
  withObjCPtr connection $ \raw_connection ->
      sendMsg nwtcpConnection (mkSelector "initWithUpgradeForConnection:") (retPtr retVoid) [argPtr (castPtr raw_connection :: Ptr ())] >>= ownedObject . castPtr

-- | cancel:
--
-- Cancel the connection. This will clean up the resources associated with this object 		and transition this object to NWTCPConnectionStateCancelled state.
--
-- ObjC selector: @- cancel@
cancel :: IsNWTCPConnection nwtcpConnection => nwtcpConnection -> IO ()
cancel nwtcpConnection  =
    sendMsg nwtcpConnection (mkSelector "cancel") retVoid []

-- | readLength:completionHandler:
--
-- Read "length" number of bytes. See readMinimumLength:maximumLength:completionHandler: 		for a complete discussion of the callback behavior.
--
-- @length@ — The exact number of bytes the application wants to read
--
-- @completion@ — The completion handler to be invoked when there is data to read or an error occurred
--
-- ObjC selector: @- readLength:completionHandler:@
readLength_completionHandler :: IsNWTCPConnection nwtcpConnection => nwtcpConnection -> CULong -> Ptr () -> IO ()
readLength_completionHandler nwtcpConnection  length_ completion =
    sendMsg nwtcpConnection (mkSelector "readLength:completionHandler:") retVoid [argCULong length_, argPtr (castPtr completion :: Ptr ())]

-- | readMinimumLength:maximumLength:completionHandler:
--
-- Read the requested range of bytes. The completion handler will be invoked when: 		- Exactly "length" number of bytes have been read. 'data' will be non-nil.
--
-- - Fewer than "length" number of bytes, including 0 bytes, have been read, and the connection's 		read side has been closed. 'data' might be nil, depending on whether there was any data to be 		read when the connection's read side was closed.
--
-- - Some fatal error has occurred, and 'data' will be nil.
--
-- To know when to schedule a read again, check for the condition whether an error has occurred.
--
-- For better performance, the caller should pick the effective minimum and maximum lengths.		For example, if the caller absolutely needs a specific number of bytes before it can		make any progress, use that value as the minimum. The maximum bytes can be the upperbound		that the caller wants to read. Typically, the minimum length can be the caller		protocol fixed-size header and the maximum length can be the maximum size of the payload or		the size of the current read buffer.
--
-- @minimum@ — The minimum number of bytes the caller wants to read
--
-- @maximum@ — The maximum number of bytes the caller wants to read
--
-- @completion@ — The completion handler to be invoked when there is data to read or an error occurred
--
-- ObjC selector: @- readMinimumLength:maximumLength:completionHandler:@
readMinimumLength_maximumLength_completionHandler :: IsNWTCPConnection nwtcpConnection => nwtcpConnection -> CULong -> CULong -> Ptr () -> IO ()
readMinimumLength_maximumLength_completionHandler nwtcpConnection  minimum_ maximum_ completion =
    sendMsg nwtcpConnection (mkSelector "readMinimumLength:maximumLength:completionHandler:") retVoid [argCULong minimum_, argCULong maximum_, argPtr (castPtr completion :: Ptr ())]

-- | write:completionHandler:
--
-- Write the given data object content. Callers should wait until the completionHandler is executed		before issuing another write.
--
-- @data@ — The data object whose content will be written
--
-- @completion@ — The completion handler to be invoked when the data content has been written or an error has occurred. 		If the error is nil, the write succeeded and the caller can write more data.
--
-- ObjC selector: @- write:completionHandler:@
write_completionHandler :: (IsNWTCPConnection nwtcpConnection, IsNSData data_) => nwtcpConnection -> data_ -> Ptr () -> IO ()
write_completionHandler nwtcpConnection  data_ completion =
  withObjCPtr data_ $ \raw_data_ ->
      sendMsg nwtcpConnection (mkSelector "write:completionHandler:") retVoid [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | writeClose:
--
-- Close this connection's write side such that further write requests won't succeed. 		Note that this has the effect of closing the read side of the peer connection. 		When the connection's read side and write side are closed, the connection is considered 		disconnected and will transition to the appropriate state.
--
-- ObjC selector: @- writeClose@
writeClose :: IsNWTCPConnection nwtcpConnection => nwtcpConnection -> IO ()
writeClose nwtcpConnection  =
    sendMsg nwtcpConnection (mkSelector "writeClose") retVoid []

-- | state
--
-- The status of the connection. Use KVO to watch this property to get updates.
--
-- ObjC selector: @- state@
state :: IsNWTCPConnection nwtcpConnection => nwtcpConnection -> IO NWTCPConnectionState
state nwtcpConnection  =
    fmap (coerce :: CLong -> NWTCPConnectionState) $ sendMsg nwtcpConnection (mkSelector "state") retCLong []

-- | viable
--
-- YES if the connection can read and write data, NO otherwise. Use KVO to watch this property.
--
-- ObjC selector: @- viable@
viable :: IsNWTCPConnection nwtcpConnection => nwtcpConnection -> IO Bool
viable nwtcpConnection  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nwtcpConnection (mkSelector "viable") retCULong []

-- | hasBetterPath
--
-- YES if the system determines there is a better path the destination can be reached if		the caller creates a new connection using the same endpoint and parameters. This can		be done using the convenience upgrade initializer method.		Use KVO to watch this property to get updates.
--
-- ObjC selector: @- hasBetterPath@
hasBetterPath :: IsNWTCPConnection nwtcpConnection => nwtcpConnection -> IO Bool
hasBetterPath nwtcpConnection  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nwtcpConnection (mkSelector "hasBetterPath") retCULong []

-- | endpoint
--
-- The destination endpoint with which this connection was created.
--
-- ObjC selector: @- endpoint@
endpoint :: IsNWTCPConnection nwtcpConnection => nwtcpConnection -> IO (Id NWEndpoint)
endpoint nwtcpConnection  =
    sendMsg nwtcpConnection (mkSelector "endpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | connectedPath
--
-- The network path over which the connection was established. The caller can query		additional properties from the NWPath object for more information.
--
-- Note that this contains a snapshot of information at the time of connection establishment 		for this connection only. As a result, some underlying properties might change in time and 		might not reflect the path for other connections that might be established at different times.
--
-- ObjC selector: @- connectedPath@
connectedPath :: IsNWTCPConnection nwtcpConnection => nwtcpConnection -> IO (Id NWPath)
connectedPath nwtcpConnection  =
    sendMsg nwtcpConnection (mkSelector "connectedPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | localAddress
--
-- The IP address endpoint from which the connection was connected.
--
-- ObjC selector: @- localAddress@
localAddress :: IsNWTCPConnection nwtcpConnection => nwtcpConnection -> IO (Id NWEndpoint)
localAddress nwtcpConnection  =
    sendMsg nwtcpConnection (mkSelector "localAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | remoteAddress
--
-- The IP address endpoint to which the connection was connected.
--
-- ObjC selector: @- remoteAddress@
remoteAddress :: IsNWTCPConnection nwtcpConnection => nwtcpConnection -> IO (Id NWEndpoint)
remoteAddress nwtcpConnection  =
    sendMsg nwtcpConnection (mkSelector "remoteAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | txtRecord
--
-- When the connection is connected to a Bonjour service endpoint, the TXT record associated 		with the Bonjour service is available via this property. Beware that the value comes from 		the network. Care must be taken when parsing this potentially malicious value.
--
-- ObjC selector: @- txtRecord@
txtRecord :: IsNWTCPConnection nwtcpConnection => nwtcpConnection -> IO (Id NSData)
txtRecord nwtcpConnection  =
    sendMsg nwtcpConnection (mkSelector "txtRecord") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | error
--
-- The connection-wide error property indicates any fatal error that occurred while 		processing the connection or performing data reading or writing.
--
-- ObjC selector: @- error@
error_ :: IsNWTCPConnection nwtcpConnection => nwtcpConnection -> IO (Id NSError)
error_ nwtcpConnection  =
    sendMsg nwtcpConnection (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUpgradeForConnection:@
initWithUpgradeForConnectionSelector :: Selector
initWithUpgradeForConnectionSelector = mkSelector "initWithUpgradeForConnection:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @readLength:completionHandler:@
readLength_completionHandlerSelector :: Selector
readLength_completionHandlerSelector = mkSelector "readLength:completionHandler:"

-- | @Selector@ for @readMinimumLength:maximumLength:completionHandler:@
readMinimumLength_maximumLength_completionHandlerSelector :: Selector
readMinimumLength_maximumLength_completionHandlerSelector = mkSelector "readMinimumLength:maximumLength:completionHandler:"

-- | @Selector@ for @write:completionHandler:@
write_completionHandlerSelector :: Selector
write_completionHandlerSelector = mkSelector "write:completionHandler:"

-- | @Selector@ for @writeClose@
writeCloseSelector :: Selector
writeCloseSelector = mkSelector "writeClose"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @viable@
viableSelector :: Selector
viableSelector = mkSelector "viable"

-- | @Selector@ for @hasBetterPath@
hasBetterPathSelector :: Selector
hasBetterPathSelector = mkSelector "hasBetterPath"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @connectedPath@
connectedPathSelector :: Selector
connectedPathSelector = mkSelector "connectedPath"

-- | @Selector@ for @localAddress@
localAddressSelector :: Selector
localAddressSelector = mkSelector "localAddress"

-- | @Selector@ for @remoteAddress@
remoteAddressSelector :: Selector
remoteAddressSelector = mkSelector "remoteAddress"

-- | @Selector@ for @txtRecord@
txtRecordSelector :: Selector
txtRecordSelector = mkSelector "txtRecord"

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

