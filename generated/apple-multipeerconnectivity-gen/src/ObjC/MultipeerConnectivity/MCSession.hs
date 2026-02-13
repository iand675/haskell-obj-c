{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MCSession@.
module ObjC.MultipeerConnectivity.MCSession
  ( MCSession
  , IsMCSession(..)
  , initWithPeer
  , initWithPeer_securityIdentity_encryptionPreference
  , sendData_toPeers_withMode_error
  , disconnect
  , sendResourceAtURL_withName_toPeer_withCompletionHandler
  , startStreamWithName_toPeer_error
  , nearbyConnectionDataForPeer_withCompletionHandler
  , connectPeer_withNearbyConnectionData
  , cancelConnectPeer
  , delegate
  , setDelegate
  , myPeerID
  , securityIdentity
  , encryptionPreference
  , connectedPeers
  , cancelConnectPeerSelector
  , connectPeer_withNearbyConnectionDataSelector
  , connectedPeersSelector
  , delegateSelector
  , disconnectSelector
  , encryptionPreferenceSelector
  , initWithPeerSelector
  , initWithPeer_securityIdentity_encryptionPreferenceSelector
  , myPeerIDSelector
  , nearbyConnectionDataForPeer_withCompletionHandlerSelector
  , securityIdentitySelector
  , sendData_toPeers_withMode_errorSelector
  , sendResourceAtURL_withName_toPeer_withCompletionHandlerSelector
  , setDelegateSelector
  , startStreamWithName_toPeer_errorSelector

  -- * Enum types
  , MCEncryptionPreference(MCEncryptionPreference)
  , pattern MCEncryptionOptional
  , pattern MCEncryptionRequired
  , pattern MCEncryptionNone
  , MCSessionSendDataMode(MCSessionSendDataMode)
  , pattern MCSessionSendDataReliable
  , pattern MCSessionSendDataUnreliable

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MultipeerConnectivity.Internal.Classes
import ObjC.MultipeerConnectivity.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithPeer:@
initWithPeer :: (IsMCSession mcSession, IsMCPeerID myPeerID) => mcSession -> myPeerID -> IO (Id MCSession)
initWithPeer mcSession myPeerID =
  sendOwnedMessage mcSession initWithPeerSelector (toMCPeerID myPeerID)

-- | @- initWithPeer:securityIdentity:encryptionPreference:@
initWithPeer_securityIdentity_encryptionPreference :: (IsMCSession mcSession, IsMCPeerID myPeerID, IsNSArray identity) => mcSession -> myPeerID -> identity -> MCEncryptionPreference -> IO (Id MCSession)
initWithPeer_securityIdentity_encryptionPreference mcSession myPeerID identity encryptionPreference =
  sendOwnedMessage mcSession initWithPeer_securityIdentity_encryptionPreferenceSelector (toMCPeerID myPeerID) (toNSArray identity) encryptionPreference

-- | @- sendData:toPeers:withMode:error:@
sendData_toPeers_withMode_error :: (IsMCSession mcSession, IsNSData data_, IsNSArray peerIDs, IsNSError error_) => mcSession -> data_ -> peerIDs -> MCSessionSendDataMode -> error_ -> IO Bool
sendData_toPeers_withMode_error mcSession data_ peerIDs mode error_ =
  sendMessage mcSession sendData_toPeers_withMode_errorSelector (toNSData data_) (toNSArray peerIDs) mode (toNSError error_)

-- | @- disconnect@
disconnect :: IsMCSession mcSession => mcSession -> IO ()
disconnect mcSession =
  sendMessage mcSession disconnectSelector

-- | @- sendResourceAtURL:withName:toPeer:withCompletionHandler:@
sendResourceAtURL_withName_toPeer_withCompletionHandler :: (IsMCSession mcSession, IsNSURL resourceURL, IsNSString resourceName, IsMCPeerID peerID) => mcSession -> resourceURL -> resourceName -> peerID -> Ptr () -> IO (Id NSProgress)
sendResourceAtURL_withName_toPeer_withCompletionHandler mcSession resourceURL resourceName peerID completionHandler =
  sendMessage mcSession sendResourceAtURL_withName_toPeer_withCompletionHandlerSelector (toNSURL resourceURL) (toNSString resourceName) (toMCPeerID peerID) completionHandler

-- | @- startStreamWithName:toPeer:error:@
startStreamWithName_toPeer_error :: (IsMCSession mcSession, IsNSString streamName, IsMCPeerID peerID, IsNSError error_) => mcSession -> streamName -> peerID -> error_ -> IO (Id NSOutputStream)
startStreamWithName_toPeer_error mcSession streamName peerID error_ =
  sendMessage mcSession startStreamWithName_toPeer_errorSelector (toNSString streamName) (toMCPeerID peerID) (toNSError error_)

-- | @- nearbyConnectionDataForPeer:withCompletionHandler:@
nearbyConnectionDataForPeer_withCompletionHandler :: (IsMCSession mcSession, IsMCPeerID peerID) => mcSession -> peerID -> Ptr () -> IO ()
nearbyConnectionDataForPeer_withCompletionHandler mcSession peerID completionHandler =
  sendMessage mcSession nearbyConnectionDataForPeer_withCompletionHandlerSelector (toMCPeerID peerID) completionHandler

-- | @- connectPeer:withNearbyConnectionData:@
connectPeer_withNearbyConnectionData :: (IsMCSession mcSession, IsMCPeerID peerID, IsNSData data_) => mcSession -> peerID -> data_ -> IO ()
connectPeer_withNearbyConnectionData mcSession peerID data_ =
  sendMessage mcSession connectPeer_withNearbyConnectionDataSelector (toMCPeerID peerID) (toNSData data_)

-- | @- cancelConnectPeer:@
cancelConnectPeer :: (IsMCSession mcSession, IsMCPeerID peerID) => mcSession -> peerID -> IO ()
cancelConnectPeer mcSession peerID =
  sendMessage mcSession cancelConnectPeerSelector (toMCPeerID peerID)

-- | @- delegate@
delegate :: IsMCSession mcSession => mcSession -> IO RawId
delegate mcSession =
  sendMessage mcSession delegateSelector

-- | @- setDelegate:@
setDelegate :: IsMCSession mcSession => mcSession -> RawId -> IO ()
setDelegate mcSession value =
  sendMessage mcSession setDelegateSelector value

-- | @- myPeerID@
myPeerID :: IsMCSession mcSession => mcSession -> IO (Id MCPeerID)
myPeerID mcSession =
  sendMessage mcSession myPeerIDSelector

-- | @- securityIdentity@
securityIdentity :: IsMCSession mcSession => mcSession -> IO (Id NSArray)
securityIdentity mcSession =
  sendMessage mcSession securityIdentitySelector

-- | @- encryptionPreference@
encryptionPreference :: IsMCSession mcSession => mcSession -> IO MCEncryptionPreference
encryptionPreference mcSession =
  sendMessage mcSession encryptionPreferenceSelector

-- | @- connectedPeers@
connectedPeers :: IsMCSession mcSession => mcSession -> IO (Id NSArray)
connectedPeers mcSession =
  sendMessage mcSession connectedPeersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPeer:@
initWithPeerSelector :: Selector '[Id MCPeerID] (Id MCSession)
initWithPeerSelector = mkSelector "initWithPeer:"

-- | @Selector@ for @initWithPeer:securityIdentity:encryptionPreference:@
initWithPeer_securityIdentity_encryptionPreferenceSelector :: Selector '[Id MCPeerID, Id NSArray, MCEncryptionPreference] (Id MCSession)
initWithPeer_securityIdentity_encryptionPreferenceSelector = mkSelector "initWithPeer:securityIdentity:encryptionPreference:"

-- | @Selector@ for @sendData:toPeers:withMode:error:@
sendData_toPeers_withMode_errorSelector :: Selector '[Id NSData, Id NSArray, MCSessionSendDataMode, Id NSError] Bool
sendData_toPeers_withMode_errorSelector = mkSelector "sendData:toPeers:withMode:error:"

-- | @Selector@ for @disconnect@
disconnectSelector :: Selector '[] ()
disconnectSelector = mkSelector "disconnect"

-- | @Selector@ for @sendResourceAtURL:withName:toPeer:withCompletionHandler:@
sendResourceAtURL_withName_toPeer_withCompletionHandlerSelector :: Selector '[Id NSURL, Id NSString, Id MCPeerID, Ptr ()] (Id NSProgress)
sendResourceAtURL_withName_toPeer_withCompletionHandlerSelector = mkSelector "sendResourceAtURL:withName:toPeer:withCompletionHandler:"

-- | @Selector@ for @startStreamWithName:toPeer:error:@
startStreamWithName_toPeer_errorSelector :: Selector '[Id NSString, Id MCPeerID, Id NSError] (Id NSOutputStream)
startStreamWithName_toPeer_errorSelector = mkSelector "startStreamWithName:toPeer:error:"

-- | @Selector@ for @nearbyConnectionDataForPeer:withCompletionHandler:@
nearbyConnectionDataForPeer_withCompletionHandlerSelector :: Selector '[Id MCPeerID, Ptr ()] ()
nearbyConnectionDataForPeer_withCompletionHandlerSelector = mkSelector "nearbyConnectionDataForPeer:withCompletionHandler:"

-- | @Selector@ for @connectPeer:withNearbyConnectionData:@
connectPeer_withNearbyConnectionDataSelector :: Selector '[Id MCPeerID, Id NSData] ()
connectPeer_withNearbyConnectionDataSelector = mkSelector "connectPeer:withNearbyConnectionData:"

-- | @Selector@ for @cancelConnectPeer:@
cancelConnectPeerSelector :: Selector '[Id MCPeerID] ()
cancelConnectPeerSelector = mkSelector "cancelConnectPeer:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @myPeerID@
myPeerIDSelector :: Selector '[] (Id MCPeerID)
myPeerIDSelector = mkSelector "myPeerID"

-- | @Selector@ for @securityIdentity@
securityIdentitySelector :: Selector '[] (Id NSArray)
securityIdentitySelector = mkSelector "securityIdentity"

-- | @Selector@ for @encryptionPreference@
encryptionPreferenceSelector :: Selector '[] MCEncryptionPreference
encryptionPreferenceSelector = mkSelector "encryptionPreference"

-- | @Selector@ for @connectedPeers@
connectedPeersSelector :: Selector '[] (Id NSArray)
connectedPeersSelector = mkSelector "connectedPeers"

