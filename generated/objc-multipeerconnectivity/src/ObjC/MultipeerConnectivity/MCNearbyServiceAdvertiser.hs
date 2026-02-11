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
  , myPeerID
  , discoveryInfo
  , serviceType
  , initWithPeer_discoveryInfo_serviceTypeSelector
  , startAdvertisingPeerSelector
  , stopAdvertisingPeerSelector
  , myPeerIDSelector
  , discoveryInfoSelector
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

-- | @- initWithPeer:discoveryInfo:serviceType:@
initWithPeer_discoveryInfo_serviceType :: (IsMCNearbyServiceAdvertiser mcNearbyServiceAdvertiser, IsMCPeerID myPeerID, IsNSDictionary info, IsNSString serviceType) => mcNearbyServiceAdvertiser -> myPeerID -> info -> serviceType -> IO (Id MCNearbyServiceAdvertiser)
initWithPeer_discoveryInfo_serviceType mcNearbyServiceAdvertiser  myPeerID info serviceType =
withObjCPtr myPeerID $ \raw_myPeerID ->
  withObjCPtr info $ \raw_info ->
    withObjCPtr serviceType $ \raw_serviceType ->
        sendMsg mcNearbyServiceAdvertiser (mkSelector "initWithPeer:discoveryInfo:serviceType:") (retPtr retVoid) [argPtr (castPtr raw_myPeerID :: Ptr ()), argPtr (castPtr raw_info :: Ptr ()), argPtr (castPtr raw_serviceType :: Ptr ())] >>= ownedObject . castPtr

-- | @- startAdvertisingPeer@
startAdvertisingPeer :: IsMCNearbyServiceAdvertiser mcNearbyServiceAdvertiser => mcNearbyServiceAdvertiser -> IO ()
startAdvertisingPeer mcNearbyServiceAdvertiser  =
  sendMsg mcNearbyServiceAdvertiser (mkSelector "startAdvertisingPeer") retVoid []

-- | @- stopAdvertisingPeer@
stopAdvertisingPeer :: IsMCNearbyServiceAdvertiser mcNearbyServiceAdvertiser => mcNearbyServiceAdvertiser -> IO ()
stopAdvertisingPeer mcNearbyServiceAdvertiser  =
  sendMsg mcNearbyServiceAdvertiser (mkSelector "stopAdvertisingPeer") retVoid []

-- | @- myPeerID@
myPeerID :: IsMCNearbyServiceAdvertiser mcNearbyServiceAdvertiser => mcNearbyServiceAdvertiser -> IO (Id MCPeerID)
myPeerID mcNearbyServiceAdvertiser  =
  sendMsg mcNearbyServiceAdvertiser (mkSelector "myPeerID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- discoveryInfo@
discoveryInfo :: IsMCNearbyServiceAdvertiser mcNearbyServiceAdvertiser => mcNearbyServiceAdvertiser -> IO (Id NSDictionary)
discoveryInfo mcNearbyServiceAdvertiser  =
  sendMsg mcNearbyServiceAdvertiser (mkSelector "discoveryInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- serviceType@
serviceType :: IsMCNearbyServiceAdvertiser mcNearbyServiceAdvertiser => mcNearbyServiceAdvertiser -> IO (Id NSString)
serviceType mcNearbyServiceAdvertiser  =
  sendMsg mcNearbyServiceAdvertiser (mkSelector "serviceType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPeer:discoveryInfo:serviceType:@
initWithPeer_discoveryInfo_serviceTypeSelector :: Selector
initWithPeer_discoveryInfo_serviceTypeSelector = mkSelector "initWithPeer:discoveryInfo:serviceType:"

-- | @Selector@ for @startAdvertisingPeer@
startAdvertisingPeerSelector :: Selector
startAdvertisingPeerSelector = mkSelector "startAdvertisingPeer"

-- | @Selector@ for @stopAdvertisingPeer@
stopAdvertisingPeerSelector :: Selector
stopAdvertisingPeerSelector = mkSelector "stopAdvertisingPeer"

-- | @Selector@ for @myPeerID@
myPeerIDSelector :: Selector
myPeerIDSelector = mkSelector "myPeerID"

-- | @Selector@ for @discoveryInfo@
discoveryInfoSelector :: Selector
discoveryInfoSelector = mkSelector "discoveryInfo"

-- | @Selector@ for @serviceType@
serviceTypeSelector :: Selector
serviceTypeSelector = mkSelector "serviceType"

