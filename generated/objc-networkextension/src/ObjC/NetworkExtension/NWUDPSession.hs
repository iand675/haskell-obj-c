{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NWUDPSession
--
-- Open UDP datagram sessions to an endpoint, and send and receive datagrams.
--
-- Generated bindings for @NWUDPSession@.
module ObjC.NetworkExtension.NWUDPSession
  ( NWUDPSession
  , IsNWUDPSession(..)
  , initWithUpgradeForSession
  , tryNextResolvedEndpoint
  , writeMultipleDatagrams_completionHandler
  , writeDatagram_completionHandler
  , cancel
  , state
  , viable
  , hasBetterPath
  , maximumDatagramLength
  , initWithUpgradeForSessionSelector
  , tryNextResolvedEndpointSelector
  , writeMultipleDatagrams_completionHandlerSelector
  , writeDatagram_completionHandlerSelector
  , cancelSelector
  , stateSelector
  , viableSelector
  , hasBetterPathSelector
  , maximumDatagramLengthSelector

  -- * Enum types
  , NWUDPSessionState(NWUDPSessionState)
  , pattern NWUDPSessionStateInvalid
  , pattern NWUDPSessionStateWaiting
  , pattern NWUDPSessionStatePreparing
  , pattern NWUDPSessionStateReady
  , pattern NWUDPSessionStateFailed
  , pattern NWUDPSessionStateCancelled

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

-- | initWithUpgradeForSession:
--
-- This convenience initializer can be used to create a new session based on the		original session's endpoint and parameters.
--
-- The application should create an NWUDPSession and watch the "hasBetterPath" property.		When this property is YES, it should call initWithUpgradeForSession: to create a new		session, with the goal to start transferring data on the new better path as soon as		possible to reduce power and potentially monetary cost. When the new "upgrade" session		becomes ready and when the application wraps up the previous application session on		the original session, the application can start using the new "upgrade" session and		tear down the original one.
--
-- @session@ — The original session from which the application will upgrade
--
-- Returns: An initialized NWUDPSession object.
--
-- ObjC selector: @- initWithUpgradeForSession:@
initWithUpgradeForSession :: (IsNWUDPSession nwudpSession, IsNWUDPSession session) => nwudpSession -> session -> IO (Id NWUDPSession)
initWithUpgradeForSession nwudpSession  session =
withObjCPtr session $ \raw_session ->
    sendMsg nwudpSession (mkSelector "initWithUpgradeForSession:") (retPtr retVoid) [argPtr (castPtr raw_session :: Ptr ())] >>= ownedObject . castPtr

-- | tryNextResolvedEndpoint
--
-- Mark the current value of resolvedEndpoint as unusable, and try to switch to the		next available endpoint. This should be used when the caller has attempted to communicate		with the current resolvedEndpoint, and the caller has determined that it is unusable. If		there are no other resolved endpoints, the session will move to the failed state.
--
-- ObjC selector: @- tryNextResolvedEndpoint@
tryNextResolvedEndpoint :: IsNWUDPSession nwudpSession => nwudpSession -> IO ()
tryNextResolvedEndpoint nwudpSession  =
  sendMsg nwudpSession (mkSelector "tryNextResolvedEndpoint") retVoid []

-- | writeMultipleDatagrams:completionHandler
--
-- Write multiple datagrams. Callers should wait until the completionHandler is executed		before issuing another write.
--
-- @datagramArray@ — An NSArray of NSData objects, containing the ordered list datagrams to write.
--
-- @completionHandler@ — A handler called when the write request has either succeeded or failed.
--
-- ObjC selector: @- writeMultipleDatagrams:completionHandler:@
writeMultipleDatagrams_completionHandler :: (IsNWUDPSession nwudpSession, IsNSArray datagramArray) => nwudpSession -> datagramArray -> Ptr () -> IO ()
writeMultipleDatagrams_completionHandler nwudpSession  datagramArray completionHandler =
withObjCPtr datagramArray $ \raw_datagramArray ->
    sendMsg nwudpSession (mkSelector "writeMultipleDatagrams:completionHandler:") retVoid [argPtr (castPtr raw_datagramArray :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | writeDatagram:completionHandler
--
-- Write a single datagram. Callers should wait until the completionHandler is executed		before issuing another write.
--
-- @datagram@ — An NSData containing the datagram to write.
--
-- @completionHandler@ — A handler called when the write request has either succeeded or failed.
--
-- ObjC selector: @- writeDatagram:completionHandler:@
writeDatagram_completionHandler :: (IsNWUDPSession nwudpSession, IsNSData datagram) => nwudpSession -> datagram -> Ptr () -> IO ()
writeDatagram_completionHandler nwudpSession  datagram completionHandler =
withObjCPtr datagram $ \raw_datagram ->
    sendMsg nwudpSession (mkSelector "writeDatagram:completionHandler:") retVoid [argPtr (castPtr raw_datagram :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | cancel
--
-- Move into the NWUDPSessionStateCancelled state. The connection will be terminated,		and all handlers will be cancelled.
--
-- ObjC selector: @- cancel@
cancel :: IsNWUDPSession nwudpSession => nwudpSession -> IO ()
cancel nwudpSession  =
  sendMsg nwudpSession (mkSelector "cancel") retVoid []

-- | state
--
-- The current state of the UDP session. If the state is NWUDPSessionStateReady,		then the connection is eligible for reading and writing. The state will be		NWUDPSessionStateFailed if the endpoint could not be resolved, or all endpoints have been		rejected. Use KVO to watch for changes.
--
-- ObjC selector: @- state@
state :: IsNWUDPSession nwudpSession => nwudpSession -> IO NWUDPSessionState
state nwudpSession  =
  fmap (coerce :: CLong -> NWUDPSessionState) $ sendMsg nwudpSession (mkSelector "state") retCLong []

-- | viable
--
-- YES if the connection can read and write data, NO otherwise.		Use KVO to watch this property.
--
-- ObjC selector: @- viable@
viable :: IsNWUDPSession nwudpSession => nwudpSession -> IO Bool
viable nwudpSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nwudpSession (mkSelector "viable") retCULong []

-- | hasBetterPath
--
-- YES if there is another path available that is preferred over the currentPath.		To take advantage of this path, create a new UDPSession. Use KVO to watch for changes.
--
-- ObjC selector: @- hasBetterPath@
hasBetterPath :: IsNWUDPSession nwudpSession => nwudpSession -> IO Bool
hasBetterPath nwudpSession  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nwudpSession (mkSelector "hasBetterPath") retCULong []

-- | maximumDatagramLength
--
-- The maximum size of a datagram to be written currently. If a datagram is written		with a longer length, the datagram may be fragmented or encounter an error. Note that this		value is not guaranteed to be the maximum datagram length for end-to-end communication		across the network. Use KVO to watch for changes.
--
-- ObjC selector: @- maximumDatagramLength@
maximumDatagramLength :: IsNWUDPSession nwudpSession => nwudpSession -> IO CULong
maximumDatagramLength nwudpSession  =
  sendMsg nwudpSession (mkSelector "maximumDatagramLength") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUpgradeForSession:@
initWithUpgradeForSessionSelector :: Selector
initWithUpgradeForSessionSelector = mkSelector "initWithUpgradeForSession:"

-- | @Selector@ for @tryNextResolvedEndpoint@
tryNextResolvedEndpointSelector :: Selector
tryNextResolvedEndpointSelector = mkSelector "tryNextResolvedEndpoint"

-- | @Selector@ for @writeMultipleDatagrams:completionHandler:@
writeMultipleDatagrams_completionHandlerSelector :: Selector
writeMultipleDatagrams_completionHandlerSelector = mkSelector "writeMultipleDatagrams:completionHandler:"

-- | @Selector@ for @writeDatagram:completionHandler:@
writeDatagram_completionHandlerSelector :: Selector
writeDatagram_completionHandlerSelector = mkSelector "writeDatagram:completionHandler:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @viable@
viableSelector :: Selector
viableSelector = mkSelector "viable"

-- | @Selector@ for @hasBetterPath@
hasBetterPathSelector :: Selector
hasBetterPathSelector = mkSelector "hasBetterPath"

-- | @Selector@ for @maximumDatagramLength@
maximumDatagramLengthSelector :: Selector
maximumDatagramLengthSelector = mkSelector "maximumDatagramLength"

