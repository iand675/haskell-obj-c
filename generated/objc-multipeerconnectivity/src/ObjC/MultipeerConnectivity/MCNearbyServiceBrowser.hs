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
  , myPeerID
  , serviceType
  , initWithPeer_serviceTypeSelector
  , startBrowsingForPeersSelector
  , stopBrowsingForPeersSelector
  , invitePeer_toSession_withContext_timeoutSelector
  , myPeerIDSelector
  , serviceTypeSelector


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

import ObjC.MultipeerConnectivity.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPeer:serviceType:@
initWithPeer_serviceType :: (IsMCNearbyServiceBrowser mcNearbyServiceBrowser, IsMCPeerID myPeerID, IsNSString serviceType) => mcNearbyServiceBrowser -> myPeerID -> serviceType -> IO (Id MCNearbyServiceBrowser)
initWithPeer_serviceType mcNearbyServiceBrowser  myPeerID serviceType =
withObjCPtr myPeerID $ \raw_myPeerID ->
  withObjCPtr serviceType $ \raw_serviceType ->
      sendMsg mcNearbyServiceBrowser (mkSelector "initWithPeer:serviceType:") (retPtr retVoid) [argPtr (castPtr raw_myPeerID :: Ptr ()), argPtr (castPtr raw_serviceType :: Ptr ())] >>= ownedObject . castPtr

-- | @- startBrowsingForPeers@
startBrowsingForPeers :: IsMCNearbyServiceBrowser mcNearbyServiceBrowser => mcNearbyServiceBrowser -> IO ()
startBrowsingForPeers mcNearbyServiceBrowser  =
  sendMsg mcNearbyServiceBrowser (mkSelector "startBrowsingForPeers") retVoid []

-- | @- stopBrowsingForPeers@
stopBrowsingForPeers :: IsMCNearbyServiceBrowser mcNearbyServiceBrowser => mcNearbyServiceBrowser -> IO ()
stopBrowsingForPeers mcNearbyServiceBrowser  =
  sendMsg mcNearbyServiceBrowser (mkSelector "stopBrowsingForPeers") retVoid []

-- | @- invitePeer:toSession:withContext:timeout:@
invitePeer_toSession_withContext_timeout :: (IsMCNearbyServiceBrowser mcNearbyServiceBrowser, IsMCPeerID peerID, IsMCSession session, IsNSData context) => mcNearbyServiceBrowser -> peerID -> session -> context -> CDouble -> IO ()
invitePeer_toSession_withContext_timeout mcNearbyServiceBrowser  peerID session context timeout =
withObjCPtr peerID $ \raw_peerID ->
  withObjCPtr session $ \raw_session ->
    withObjCPtr context $ \raw_context ->
        sendMsg mcNearbyServiceBrowser (mkSelector "invitePeer:toSession:withContext:timeout:") retVoid [argPtr (castPtr raw_peerID :: Ptr ()), argPtr (castPtr raw_session :: Ptr ()), argPtr (castPtr raw_context :: Ptr ()), argCDouble (fromIntegral timeout)]

-- | @- myPeerID@
myPeerID :: IsMCNearbyServiceBrowser mcNearbyServiceBrowser => mcNearbyServiceBrowser -> IO (Id MCPeerID)
myPeerID mcNearbyServiceBrowser  =
  sendMsg mcNearbyServiceBrowser (mkSelector "myPeerID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- serviceType@
serviceType :: IsMCNearbyServiceBrowser mcNearbyServiceBrowser => mcNearbyServiceBrowser -> IO (Id NSString)
serviceType mcNearbyServiceBrowser  =
  sendMsg mcNearbyServiceBrowser (mkSelector "serviceType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPeer:serviceType:@
initWithPeer_serviceTypeSelector :: Selector
initWithPeer_serviceTypeSelector = mkSelector "initWithPeer:serviceType:"

-- | @Selector@ for @startBrowsingForPeers@
startBrowsingForPeersSelector :: Selector
startBrowsingForPeersSelector = mkSelector "startBrowsingForPeers"

-- | @Selector@ for @stopBrowsingForPeers@
stopBrowsingForPeersSelector :: Selector
stopBrowsingForPeersSelector = mkSelector "stopBrowsingForPeers"

-- | @Selector@ for @invitePeer:toSession:withContext:timeout:@
invitePeer_toSession_withContext_timeoutSelector :: Selector
invitePeer_toSession_withContext_timeoutSelector = mkSelector "invitePeer:toSession:withContext:timeout:"

-- | @Selector@ for @myPeerID@
myPeerIDSelector :: Selector
myPeerIDSelector = mkSelector "myPeerID"

-- | @Selector@ for @serviceType@
serviceTypeSelector :: Selector
serviceTypeSelector = mkSelector "serviceType"

