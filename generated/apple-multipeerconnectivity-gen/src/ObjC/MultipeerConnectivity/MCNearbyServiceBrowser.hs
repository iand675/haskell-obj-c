{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MCNearbyServiceBrowser@.
module ObjC.MultipeerConnectivity.MCNearbyServiceBrowser
  ( MCNearbyServiceBrowser
  , IsMCNearbyServiceBrowser(..)
  , initWithPeer_serviceType
  , startBrowsingForPeers
  , stopBrowsingForPeers
  , invitePeer_toSession_withContext_timeout
  , delegate
  , setDelegate
  , myPeerID
  , serviceType
  , delegateSelector
  , initWithPeer_serviceTypeSelector
  , invitePeer_toSession_withContext_timeoutSelector
  , myPeerIDSelector
  , serviceTypeSelector
  , setDelegateSelector
  , startBrowsingForPeersSelector
  , stopBrowsingForPeersSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MultipeerConnectivity.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPeer:serviceType:@
initWithPeer_serviceType :: (IsMCNearbyServiceBrowser mcNearbyServiceBrowser, IsMCPeerID myPeerID, IsNSString serviceType) => mcNearbyServiceBrowser -> myPeerID -> serviceType -> IO (Id MCNearbyServiceBrowser)
initWithPeer_serviceType mcNearbyServiceBrowser myPeerID serviceType =
  sendOwnedMessage mcNearbyServiceBrowser initWithPeer_serviceTypeSelector (toMCPeerID myPeerID) (toNSString serviceType)

-- | @- startBrowsingForPeers@
startBrowsingForPeers :: IsMCNearbyServiceBrowser mcNearbyServiceBrowser => mcNearbyServiceBrowser -> IO ()
startBrowsingForPeers mcNearbyServiceBrowser =
  sendMessage mcNearbyServiceBrowser startBrowsingForPeersSelector

-- | @- stopBrowsingForPeers@
stopBrowsingForPeers :: IsMCNearbyServiceBrowser mcNearbyServiceBrowser => mcNearbyServiceBrowser -> IO ()
stopBrowsingForPeers mcNearbyServiceBrowser =
  sendMessage mcNearbyServiceBrowser stopBrowsingForPeersSelector

-- | @- invitePeer:toSession:withContext:timeout:@
invitePeer_toSession_withContext_timeout :: (IsMCNearbyServiceBrowser mcNearbyServiceBrowser, IsMCPeerID peerID, IsMCSession session, IsNSData context) => mcNearbyServiceBrowser -> peerID -> session -> context -> CDouble -> IO ()
invitePeer_toSession_withContext_timeout mcNearbyServiceBrowser peerID session context timeout =
  sendMessage mcNearbyServiceBrowser invitePeer_toSession_withContext_timeoutSelector (toMCPeerID peerID) (toMCSession session) (toNSData context) timeout

-- | @- delegate@
delegate :: IsMCNearbyServiceBrowser mcNearbyServiceBrowser => mcNearbyServiceBrowser -> IO RawId
delegate mcNearbyServiceBrowser =
  sendMessage mcNearbyServiceBrowser delegateSelector

-- | @- setDelegate:@
setDelegate :: IsMCNearbyServiceBrowser mcNearbyServiceBrowser => mcNearbyServiceBrowser -> RawId -> IO ()
setDelegate mcNearbyServiceBrowser value =
  sendMessage mcNearbyServiceBrowser setDelegateSelector value

-- | @- myPeerID@
myPeerID :: IsMCNearbyServiceBrowser mcNearbyServiceBrowser => mcNearbyServiceBrowser -> IO (Id MCPeerID)
myPeerID mcNearbyServiceBrowser =
  sendMessage mcNearbyServiceBrowser myPeerIDSelector

-- | @- serviceType@
serviceType :: IsMCNearbyServiceBrowser mcNearbyServiceBrowser => mcNearbyServiceBrowser -> IO (Id NSString)
serviceType mcNearbyServiceBrowser =
  sendMessage mcNearbyServiceBrowser serviceTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPeer:serviceType:@
initWithPeer_serviceTypeSelector :: Selector '[Id MCPeerID, Id NSString] (Id MCNearbyServiceBrowser)
initWithPeer_serviceTypeSelector = mkSelector "initWithPeer:serviceType:"

-- | @Selector@ for @startBrowsingForPeers@
startBrowsingForPeersSelector :: Selector '[] ()
startBrowsingForPeersSelector = mkSelector "startBrowsingForPeers"

-- | @Selector@ for @stopBrowsingForPeers@
stopBrowsingForPeersSelector :: Selector '[] ()
stopBrowsingForPeersSelector = mkSelector "stopBrowsingForPeers"

-- | @Selector@ for @invitePeer:toSession:withContext:timeout:@
invitePeer_toSession_withContext_timeoutSelector :: Selector '[Id MCPeerID, Id MCSession, Id NSData, CDouble] ()
invitePeer_toSession_withContext_timeoutSelector = mkSelector "invitePeer:toSession:withContext:timeout:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @myPeerID@
myPeerIDSelector :: Selector '[] (Id MCPeerID)
myPeerIDSelector = mkSelector "myPeerID"

-- | @Selector@ for @serviceType@
serviceTypeSelector :: Selector '[] (Id NSString)
serviceTypeSelector = mkSelector "serviceType"

