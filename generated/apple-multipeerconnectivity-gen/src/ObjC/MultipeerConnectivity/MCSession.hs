{-# LANGUAGE PatternSynonyms #-}
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
  , initWithPeerSelector
  , initWithPeer_securityIdentity_encryptionPreferenceSelector
  , sendData_toPeers_withMode_errorSelector
  , disconnectSelector
  , sendResourceAtURL_withName_toPeer_withCompletionHandlerSelector
  , startStreamWithName_toPeer_errorSelector
  , nearbyConnectionDataForPeer_withCompletionHandlerSelector
  , connectPeer_withNearbyConnectionDataSelector
  , cancelConnectPeerSelector
  , delegateSelector
  , setDelegateSelector
  , myPeerIDSelector
  , securityIdentitySelector
  , encryptionPreferenceSelector
  , connectedPeersSelector

  -- * Enum types
  , MCEncryptionPreference(MCEncryptionPreference)
  , pattern MCEncryptionOptional
  , pattern MCEncryptionRequired
  , pattern MCEncryptionNone
  , MCSessionSendDataMode(MCSessionSendDataMode)
  , pattern MCSessionSendDataReliable
  , pattern MCSessionSendDataUnreliable

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
import ObjC.MultipeerConnectivity.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithPeer:@
initWithPeer :: (IsMCSession mcSession, IsMCPeerID myPeerID) => mcSession -> myPeerID -> IO (Id MCSession)
initWithPeer mcSession  myPeerID =
  withObjCPtr myPeerID $ \raw_myPeerID ->
      sendMsg mcSession (mkSelector "initWithPeer:") (retPtr retVoid) [argPtr (castPtr raw_myPeerID :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithPeer:securityIdentity:encryptionPreference:@
initWithPeer_securityIdentity_encryptionPreference :: (IsMCSession mcSession, IsMCPeerID myPeerID, IsNSArray identity) => mcSession -> myPeerID -> identity -> MCEncryptionPreference -> IO (Id MCSession)
initWithPeer_securityIdentity_encryptionPreference mcSession  myPeerID identity encryptionPreference =
  withObjCPtr myPeerID $ \raw_myPeerID ->
    withObjCPtr identity $ \raw_identity ->
        sendMsg mcSession (mkSelector "initWithPeer:securityIdentity:encryptionPreference:") (retPtr retVoid) [argPtr (castPtr raw_myPeerID :: Ptr ()), argPtr (castPtr raw_identity :: Ptr ()), argCLong (coerce encryptionPreference)] >>= ownedObject . castPtr

-- | @- sendData:toPeers:withMode:error:@
sendData_toPeers_withMode_error :: (IsMCSession mcSession, IsNSData data_, IsNSArray peerIDs, IsNSError error_) => mcSession -> data_ -> peerIDs -> MCSessionSendDataMode -> error_ -> IO Bool
sendData_toPeers_withMode_error mcSession  data_ peerIDs mode error_ =
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr peerIDs $ \raw_peerIDs ->
      withObjCPtr error_ $ \raw_error_ ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg mcSession (mkSelector "sendData:toPeers:withMode:error:") retCULong [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_peerIDs :: Ptr ()), argCLong (coerce mode), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- disconnect@
disconnect :: IsMCSession mcSession => mcSession -> IO ()
disconnect mcSession  =
    sendMsg mcSession (mkSelector "disconnect") retVoid []

-- | @- sendResourceAtURL:withName:toPeer:withCompletionHandler:@
sendResourceAtURL_withName_toPeer_withCompletionHandler :: (IsMCSession mcSession, IsNSURL resourceURL, IsNSString resourceName, IsMCPeerID peerID) => mcSession -> resourceURL -> resourceName -> peerID -> Ptr () -> IO (Id NSProgress)
sendResourceAtURL_withName_toPeer_withCompletionHandler mcSession  resourceURL resourceName peerID completionHandler =
  withObjCPtr resourceURL $ \raw_resourceURL ->
    withObjCPtr resourceName $ \raw_resourceName ->
      withObjCPtr peerID $ \raw_peerID ->
          sendMsg mcSession (mkSelector "sendResourceAtURL:withName:toPeer:withCompletionHandler:") (retPtr retVoid) [argPtr (castPtr raw_resourceURL :: Ptr ()), argPtr (castPtr raw_resourceName :: Ptr ()), argPtr (castPtr raw_peerID :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- startStreamWithName:toPeer:error:@
startStreamWithName_toPeer_error :: (IsMCSession mcSession, IsNSString streamName, IsMCPeerID peerID, IsNSError error_) => mcSession -> streamName -> peerID -> error_ -> IO (Id NSOutputStream)
startStreamWithName_toPeer_error mcSession  streamName peerID error_ =
  withObjCPtr streamName $ \raw_streamName ->
    withObjCPtr peerID $ \raw_peerID ->
      withObjCPtr error_ $ \raw_error_ ->
          sendMsg mcSession (mkSelector "startStreamWithName:toPeer:error:") (retPtr retVoid) [argPtr (castPtr raw_streamName :: Ptr ()), argPtr (castPtr raw_peerID :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- nearbyConnectionDataForPeer:withCompletionHandler:@
nearbyConnectionDataForPeer_withCompletionHandler :: (IsMCSession mcSession, IsMCPeerID peerID) => mcSession -> peerID -> Ptr () -> IO ()
nearbyConnectionDataForPeer_withCompletionHandler mcSession  peerID completionHandler =
  withObjCPtr peerID $ \raw_peerID ->
      sendMsg mcSession (mkSelector "nearbyConnectionDataForPeer:withCompletionHandler:") retVoid [argPtr (castPtr raw_peerID :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- connectPeer:withNearbyConnectionData:@
connectPeer_withNearbyConnectionData :: (IsMCSession mcSession, IsMCPeerID peerID, IsNSData data_) => mcSession -> peerID -> data_ -> IO ()
connectPeer_withNearbyConnectionData mcSession  peerID data_ =
  withObjCPtr peerID $ \raw_peerID ->
    withObjCPtr data_ $ \raw_data_ ->
        sendMsg mcSession (mkSelector "connectPeer:withNearbyConnectionData:") retVoid [argPtr (castPtr raw_peerID :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ())]

-- | @- cancelConnectPeer:@
cancelConnectPeer :: (IsMCSession mcSession, IsMCPeerID peerID) => mcSession -> peerID -> IO ()
cancelConnectPeer mcSession  peerID =
  withObjCPtr peerID $ \raw_peerID ->
      sendMsg mcSession (mkSelector "cancelConnectPeer:") retVoid [argPtr (castPtr raw_peerID :: Ptr ())]

-- | @- delegate@
delegate :: IsMCSession mcSession => mcSession -> IO RawId
delegate mcSession  =
    fmap (RawId . castPtr) $ sendMsg mcSession (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsMCSession mcSession => mcSession -> RawId -> IO ()
setDelegate mcSession  value =
    sendMsg mcSession (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- myPeerID@
myPeerID :: IsMCSession mcSession => mcSession -> IO (Id MCPeerID)
myPeerID mcSession  =
    sendMsg mcSession (mkSelector "myPeerID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- securityIdentity@
securityIdentity :: IsMCSession mcSession => mcSession -> IO (Id NSArray)
securityIdentity mcSession  =
    sendMsg mcSession (mkSelector "securityIdentity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- encryptionPreference@
encryptionPreference :: IsMCSession mcSession => mcSession -> IO MCEncryptionPreference
encryptionPreference mcSession  =
    fmap (coerce :: CLong -> MCEncryptionPreference) $ sendMsg mcSession (mkSelector "encryptionPreference") retCLong []

-- | @- connectedPeers@
connectedPeers :: IsMCSession mcSession => mcSession -> IO (Id NSArray)
connectedPeers mcSession  =
    sendMsg mcSession (mkSelector "connectedPeers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPeer:@
initWithPeerSelector :: Selector
initWithPeerSelector = mkSelector "initWithPeer:"

-- | @Selector@ for @initWithPeer:securityIdentity:encryptionPreference:@
initWithPeer_securityIdentity_encryptionPreferenceSelector :: Selector
initWithPeer_securityIdentity_encryptionPreferenceSelector = mkSelector "initWithPeer:securityIdentity:encryptionPreference:"

-- | @Selector@ for @sendData:toPeers:withMode:error:@
sendData_toPeers_withMode_errorSelector :: Selector
sendData_toPeers_withMode_errorSelector = mkSelector "sendData:toPeers:withMode:error:"

-- | @Selector@ for @disconnect@
disconnectSelector :: Selector
disconnectSelector = mkSelector "disconnect"

-- | @Selector@ for @sendResourceAtURL:withName:toPeer:withCompletionHandler:@
sendResourceAtURL_withName_toPeer_withCompletionHandlerSelector :: Selector
sendResourceAtURL_withName_toPeer_withCompletionHandlerSelector = mkSelector "sendResourceAtURL:withName:toPeer:withCompletionHandler:"

-- | @Selector@ for @startStreamWithName:toPeer:error:@
startStreamWithName_toPeer_errorSelector :: Selector
startStreamWithName_toPeer_errorSelector = mkSelector "startStreamWithName:toPeer:error:"

-- | @Selector@ for @nearbyConnectionDataForPeer:withCompletionHandler:@
nearbyConnectionDataForPeer_withCompletionHandlerSelector :: Selector
nearbyConnectionDataForPeer_withCompletionHandlerSelector = mkSelector "nearbyConnectionDataForPeer:withCompletionHandler:"

-- | @Selector@ for @connectPeer:withNearbyConnectionData:@
connectPeer_withNearbyConnectionDataSelector :: Selector
connectPeer_withNearbyConnectionDataSelector = mkSelector "connectPeer:withNearbyConnectionData:"

-- | @Selector@ for @cancelConnectPeer:@
cancelConnectPeerSelector :: Selector
cancelConnectPeerSelector = mkSelector "cancelConnectPeer:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @myPeerID@
myPeerIDSelector :: Selector
myPeerIDSelector = mkSelector "myPeerID"

-- | @Selector@ for @securityIdentity@
securityIdentitySelector :: Selector
securityIdentitySelector = mkSelector "securityIdentity"

-- | @Selector@ for @encryptionPreference@
encryptionPreferenceSelector :: Selector
encryptionPreferenceSelector = mkSelector "encryptionPreference"

-- | @Selector@ for @connectedPeers@
connectedPeersSelector :: Selector
connectedPeersSelector = mkSelector "connectedPeers"

