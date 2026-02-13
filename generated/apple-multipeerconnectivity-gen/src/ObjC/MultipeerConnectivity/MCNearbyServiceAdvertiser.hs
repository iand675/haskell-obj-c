{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MCNearbyServiceAdvertiser@.
module ObjC.MultipeerConnectivity.MCNearbyServiceAdvertiser
  ( MCNearbyServiceAdvertiser
  , IsMCNearbyServiceAdvertiser(..)
  , initWithPeer_discoveryInfo_serviceType
  , startAdvertisingPeer
  , stopAdvertisingPeer
  , delegate
  , setDelegate
  , myPeerID
  , discoveryInfo
  , serviceType
  , delegateSelector
  , discoveryInfoSelector
  , initWithPeer_discoveryInfo_serviceTypeSelector
  , myPeerIDSelector
  , serviceTypeSelector
  , setDelegateSelector
  , startAdvertisingPeerSelector
  , stopAdvertisingPeerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MultipeerConnectivity.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPeer:discoveryInfo:serviceType:@
initWithPeer_discoveryInfo_serviceType :: (IsMCNearbyServiceAdvertiser mcNearbyServiceAdvertiser, IsMCPeerID myPeerID, IsNSDictionary info, IsNSString serviceType) => mcNearbyServiceAdvertiser -> myPeerID -> info -> serviceType -> IO (Id MCNearbyServiceAdvertiser)
initWithPeer_discoveryInfo_serviceType mcNearbyServiceAdvertiser myPeerID info serviceType =
  sendOwnedMessage mcNearbyServiceAdvertiser initWithPeer_discoveryInfo_serviceTypeSelector (toMCPeerID myPeerID) (toNSDictionary info) (toNSString serviceType)

-- | @- startAdvertisingPeer@
startAdvertisingPeer :: IsMCNearbyServiceAdvertiser mcNearbyServiceAdvertiser => mcNearbyServiceAdvertiser -> IO ()
startAdvertisingPeer mcNearbyServiceAdvertiser =
  sendMessage mcNearbyServiceAdvertiser startAdvertisingPeerSelector

-- | @- stopAdvertisingPeer@
stopAdvertisingPeer :: IsMCNearbyServiceAdvertiser mcNearbyServiceAdvertiser => mcNearbyServiceAdvertiser -> IO ()
stopAdvertisingPeer mcNearbyServiceAdvertiser =
  sendMessage mcNearbyServiceAdvertiser stopAdvertisingPeerSelector

-- | @- delegate@
delegate :: IsMCNearbyServiceAdvertiser mcNearbyServiceAdvertiser => mcNearbyServiceAdvertiser -> IO RawId
delegate mcNearbyServiceAdvertiser =
  sendMessage mcNearbyServiceAdvertiser delegateSelector

-- | @- setDelegate:@
setDelegate :: IsMCNearbyServiceAdvertiser mcNearbyServiceAdvertiser => mcNearbyServiceAdvertiser -> RawId -> IO ()
setDelegate mcNearbyServiceAdvertiser value =
  sendMessage mcNearbyServiceAdvertiser setDelegateSelector value

-- | @- myPeerID@
myPeerID :: IsMCNearbyServiceAdvertiser mcNearbyServiceAdvertiser => mcNearbyServiceAdvertiser -> IO (Id MCPeerID)
myPeerID mcNearbyServiceAdvertiser =
  sendMessage mcNearbyServiceAdvertiser myPeerIDSelector

-- | @- discoveryInfo@
discoveryInfo :: IsMCNearbyServiceAdvertiser mcNearbyServiceAdvertiser => mcNearbyServiceAdvertiser -> IO (Id NSDictionary)
discoveryInfo mcNearbyServiceAdvertiser =
  sendMessage mcNearbyServiceAdvertiser discoveryInfoSelector

-- | @- serviceType@
serviceType :: IsMCNearbyServiceAdvertiser mcNearbyServiceAdvertiser => mcNearbyServiceAdvertiser -> IO (Id NSString)
serviceType mcNearbyServiceAdvertiser =
  sendMessage mcNearbyServiceAdvertiser serviceTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPeer:discoveryInfo:serviceType:@
initWithPeer_discoveryInfo_serviceTypeSelector :: Selector '[Id MCPeerID, Id NSDictionary, Id NSString] (Id MCNearbyServiceAdvertiser)
initWithPeer_discoveryInfo_serviceTypeSelector = mkSelector "initWithPeer:discoveryInfo:serviceType:"

-- | @Selector@ for @startAdvertisingPeer@
startAdvertisingPeerSelector :: Selector '[] ()
startAdvertisingPeerSelector = mkSelector "startAdvertisingPeer"

-- | @Selector@ for @stopAdvertisingPeer@
stopAdvertisingPeerSelector :: Selector '[] ()
stopAdvertisingPeerSelector = mkSelector "stopAdvertisingPeer"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @myPeerID@
myPeerIDSelector :: Selector '[] (Id MCPeerID)
myPeerIDSelector = mkSelector "myPeerID"

-- | @Selector@ for @discoveryInfo@
discoveryInfoSelector :: Selector '[] (Id NSDictionary)
discoveryInfoSelector = mkSelector "discoveryInfo"

-- | @Selector@ for @serviceType@
serviceTypeSelector :: Selector '[] (Id NSString)
serviceTypeSelector = mkSelector "serviceType"

