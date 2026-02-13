{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , endpoint
  , resolvedEndpoint
  , viable
  , hasBetterPath
  , currentPath
  , maximumDatagramLength
  , cancelSelector
  , currentPathSelector
  , endpointSelector
  , hasBetterPathSelector
  , initWithUpgradeForSessionSelector
  , maximumDatagramLengthSelector
  , resolvedEndpointSelector
  , stateSelector
  , tryNextResolvedEndpointSelector
  , viableSelector
  , writeDatagram_completionHandlerSelector
  , writeMultipleDatagrams_completionHandlerSelector

  -- * Enum types
  , NWUDPSessionState(NWUDPSessionState)
  , pattern NWUDPSessionStateInvalid
  , pattern NWUDPSessionStateWaiting
  , pattern NWUDPSessionStatePreparing
  , pattern NWUDPSessionStateReady
  , pattern NWUDPSessionStateFailed
  , pattern NWUDPSessionStateCancelled

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithUpgradeForSession nwudpSession session =
  sendOwnedMessage nwudpSession initWithUpgradeForSessionSelector (toNWUDPSession session)

-- | tryNextResolvedEndpoint
--
-- Mark the current value of resolvedEndpoint as unusable, and try to switch to the		next available endpoint. This should be used when the caller has attempted to communicate		with the current resolvedEndpoint, and the caller has determined that it is unusable. If		there are no other resolved endpoints, the session will move to the failed state.
--
-- ObjC selector: @- tryNextResolvedEndpoint@
tryNextResolvedEndpoint :: IsNWUDPSession nwudpSession => nwudpSession -> IO ()
tryNextResolvedEndpoint nwudpSession =
  sendMessage nwudpSession tryNextResolvedEndpointSelector

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
writeMultipleDatagrams_completionHandler nwudpSession datagramArray completionHandler =
  sendMessage nwudpSession writeMultipleDatagrams_completionHandlerSelector (toNSArray datagramArray) completionHandler

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
writeDatagram_completionHandler nwudpSession datagram completionHandler =
  sendMessage nwudpSession writeDatagram_completionHandlerSelector (toNSData datagram) completionHandler

-- | cancel
--
-- Move into the NWUDPSessionStateCancelled state. The connection will be terminated,		and all handlers will be cancelled.
--
-- ObjC selector: @- cancel@
cancel :: IsNWUDPSession nwudpSession => nwudpSession -> IO ()
cancel nwudpSession =
  sendMessage nwudpSession cancelSelector

-- | state
--
-- The current state of the UDP session. If the state is NWUDPSessionStateReady,		then the connection is eligible for reading and writing. The state will be		NWUDPSessionStateFailed if the endpoint could not be resolved, or all endpoints have been		rejected. Use KVO to watch for changes.
--
-- ObjC selector: @- state@
state :: IsNWUDPSession nwudpSession => nwudpSession -> IO NWUDPSessionState
state nwudpSession =
  sendMessage nwudpSession stateSelector

-- | endpoint
--
-- The provided endpoint.
--
-- ObjC selector: @- endpoint@
endpoint :: IsNWUDPSession nwudpSession => nwudpSession -> IO (Id NWEndpoint)
endpoint nwudpSession =
  sendMessage nwudpSession endpointSelector

-- | resolvedEndpoint
--
-- The currently targeted remote endpoint. Use KVO to watch for changes.
--
-- ObjC selector: @- resolvedEndpoint@
resolvedEndpoint :: IsNWUDPSession nwudpSession => nwudpSession -> IO (Id NWEndpoint)
resolvedEndpoint nwudpSession =
  sendMessage nwudpSession resolvedEndpointSelector

-- | viable
--
-- YES if the connection can read and write data, NO otherwise.		Use KVO to watch this property.
--
-- ObjC selector: @- viable@
viable :: IsNWUDPSession nwudpSession => nwudpSession -> IO Bool
viable nwudpSession =
  sendMessage nwudpSession viableSelector

-- | hasBetterPath
--
-- YES if there is another path available that is preferred over the currentPath.		To take advantage of this path, create a new UDPSession. Use KVO to watch for changes.
--
-- ObjC selector: @- hasBetterPath@
hasBetterPath :: IsNWUDPSession nwudpSession => nwudpSession -> IO Bool
hasBetterPath nwudpSession =
  sendMessage nwudpSession hasBetterPathSelector

-- | currentPath
--
-- The current evaluated path for the resolvedEndpoint. Use KVO to watch for changes.
--
-- ObjC selector: @- currentPath@
currentPath :: IsNWUDPSession nwudpSession => nwudpSession -> IO (Id NWPath)
currentPath nwudpSession =
  sendMessage nwudpSession currentPathSelector

-- | maximumDatagramLength
--
-- The maximum size of a datagram to be written currently. If a datagram is written		with a longer length, the datagram may be fragmented or encounter an error. Note that this		value is not guaranteed to be the maximum datagram length for end-to-end communication		across the network. Use KVO to watch for changes.
--
-- ObjC selector: @- maximumDatagramLength@
maximumDatagramLength :: IsNWUDPSession nwudpSession => nwudpSession -> IO CULong
maximumDatagramLength nwudpSession =
  sendMessage nwudpSession maximumDatagramLengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUpgradeForSession:@
initWithUpgradeForSessionSelector :: Selector '[Id NWUDPSession] (Id NWUDPSession)
initWithUpgradeForSessionSelector = mkSelector "initWithUpgradeForSession:"

-- | @Selector@ for @tryNextResolvedEndpoint@
tryNextResolvedEndpointSelector :: Selector '[] ()
tryNextResolvedEndpointSelector = mkSelector "tryNextResolvedEndpoint"

-- | @Selector@ for @writeMultipleDatagrams:completionHandler:@
writeMultipleDatagrams_completionHandlerSelector :: Selector '[Id NSArray, Ptr ()] ()
writeMultipleDatagrams_completionHandlerSelector = mkSelector "writeMultipleDatagrams:completionHandler:"

-- | @Selector@ for @writeDatagram:completionHandler:@
writeDatagram_completionHandlerSelector :: Selector '[Id NSData, Ptr ()] ()
writeDatagram_completionHandlerSelector = mkSelector "writeDatagram:completionHandler:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @state@
stateSelector :: Selector '[] NWUDPSessionState
stateSelector = mkSelector "state"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector '[] (Id NWEndpoint)
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @resolvedEndpoint@
resolvedEndpointSelector :: Selector '[] (Id NWEndpoint)
resolvedEndpointSelector = mkSelector "resolvedEndpoint"

-- | @Selector@ for @viable@
viableSelector :: Selector '[] Bool
viableSelector = mkSelector "viable"

-- | @Selector@ for @hasBetterPath@
hasBetterPathSelector :: Selector '[] Bool
hasBetterPathSelector = mkSelector "hasBetterPath"

-- | @Selector@ for @currentPath@
currentPathSelector :: Selector '[] (Id NWPath)
currentPathSelector = mkSelector "currentPath"

-- | @Selector@ for @maximumDatagramLength@
maximumDatagramLengthSelector :: Selector '[] CULong
maximumDatagramLengthSelector = mkSelector "maximumDatagramLength"

