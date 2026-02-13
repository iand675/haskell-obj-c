{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct@.
module ObjC.Matter.MTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct
  ( MTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct
  , IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct(..)
  , groupKeySetID
  , setGroupKeySetID
  , groupKeySecurityPolicy
  , setGroupKeySecurityPolicy
  , epochKey0
  , setEpochKey0
  , epochStartTime0
  , setEpochStartTime0
  , epochKey1
  , setEpochKey1
  , epochStartTime1
  , setEpochStartTime1
  , epochKey2
  , setEpochKey2
  , epochStartTime2
  , setEpochStartTime2
  , groupKeyMulticastPolicy
  , setGroupKeyMulticastPolicy
  , epochKey0Selector
  , epochKey1Selector
  , epochKey2Selector
  , epochStartTime0Selector
  , epochStartTime1Selector
  , epochStartTime2Selector
  , groupKeyMulticastPolicySelector
  , groupKeySecurityPolicySelector
  , groupKeySetIDSelector
  , setEpochKey0Selector
  , setEpochKey1Selector
  , setEpochKey2Selector
  , setEpochStartTime0Selector
  , setEpochStartTime1Selector
  , setEpochStartTime2Selector
  , setGroupKeyMulticastPolicySelector
  , setGroupKeySecurityPolicySelector
  , setGroupKeySetIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- groupKeySetID@
groupKeySetID :: IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> IO (Id NSNumber)
groupKeySetID mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct groupKeySetIDSelector

-- | @- setGroupKeySetID:@
setGroupKeySetID :: (IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> value -> IO ()
setGroupKeySetID mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct setGroupKeySetIDSelector (toNSNumber value)

-- | @- groupKeySecurityPolicy@
groupKeySecurityPolicy :: IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> IO (Id NSNumber)
groupKeySecurityPolicy mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct groupKeySecurityPolicySelector

-- | @- setGroupKeySecurityPolicy:@
setGroupKeySecurityPolicy :: (IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> value -> IO ()
setGroupKeySecurityPolicy mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct setGroupKeySecurityPolicySelector (toNSNumber value)

-- | @- epochKey0@
epochKey0 :: IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> IO (Id NSData)
epochKey0 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct epochKey0Selector

-- | @- setEpochKey0:@
setEpochKey0 :: (IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct, IsNSData value) => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> value -> IO ()
setEpochKey0 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct setEpochKey0Selector (toNSData value)

-- | @- epochStartTime0@
epochStartTime0 :: IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> IO (Id NSNumber)
epochStartTime0 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct epochStartTime0Selector

-- | @- setEpochStartTime0:@
setEpochStartTime0 :: (IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> value -> IO ()
setEpochStartTime0 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct setEpochStartTime0Selector (toNSNumber value)

-- | @- epochKey1@
epochKey1 :: IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> IO (Id NSData)
epochKey1 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct epochKey1Selector

-- | @- setEpochKey1:@
setEpochKey1 :: (IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct, IsNSData value) => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> value -> IO ()
setEpochKey1 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct setEpochKey1Selector (toNSData value)

-- | @- epochStartTime1@
epochStartTime1 :: IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> IO (Id NSNumber)
epochStartTime1 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct epochStartTime1Selector

-- | @- setEpochStartTime1:@
setEpochStartTime1 :: (IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> value -> IO ()
setEpochStartTime1 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct setEpochStartTime1Selector (toNSNumber value)

-- | @- epochKey2@
epochKey2 :: IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> IO (Id NSData)
epochKey2 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct epochKey2Selector

-- | @- setEpochKey2:@
setEpochKey2 :: (IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct, IsNSData value) => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> value -> IO ()
setEpochKey2 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct setEpochKey2Selector (toNSData value)

-- | @- epochStartTime2@
epochStartTime2 :: IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> IO (Id NSNumber)
epochStartTime2 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct epochStartTime2Selector

-- | @- setEpochStartTime2:@
setEpochStartTime2 :: (IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> value -> IO ()
setEpochStartTime2 mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct setEpochStartTime2Selector (toNSNumber value)

-- | @- groupKeyMulticastPolicy@
groupKeyMulticastPolicy :: IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> IO (Id NSNumber)
groupKeyMulticastPolicy mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct groupKeyMulticastPolicySelector

-- | @- setGroupKeyMulticastPolicy:@
setGroupKeyMulticastPolicy :: (IsMTRJointFabricDatastoreClusterDatastoreGroupKeySetStruct mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct, IsNSNumber value) => mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct -> value -> IO ()
setGroupKeyMulticastPolicy mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct value =
  sendMessage mtrJointFabricDatastoreClusterDatastoreGroupKeySetStruct setGroupKeyMulticastPolicySelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupKeySetID@
groupKeySetIDSelector :: Selector '[] (Id NSNumber)
groupKeySetIDSelector = mkSelector "groupKeySetID"

-- | @Selector@ for @setGroupKeySetID:@
setGroupKeySetIDSelector :: Selector '[Id NSNumber] ()
setGroupKeySetIDSelector = mkSelector "setGroupKeySetID:"

-- | @Selector@ for @groupKeySecurityPolicy@
groupKeySecurityPolicySelector :: Selector '[] (Id NSNumber)
groupKeySecurityPolicySelector = mkSelector "groupKeySecurityPolicy"

-- | @Selector@ for @setGroupKeySecurityPolicy:@
setGroupKeySecurityPolicySelector :: Selector '[Id NSNumber] ()
setGroupKeySecurityPolicySelector = mkSelector "setGroupKeySecurityPolicy:"

-- | @Selector@ for @epochKey0@
epochKey0Selector :: Selector '[] (Id NSData)
epochKey0Selector = mkSelector "epochKey0"

-- | @Selector@ for @setEpochKey0:@
setEpochKey0Selector :: Selector '[Id NSData] ()
setEpochKey0Selector = mkSelector "setEpochKey0:"

-- | @Selector@ for @epochStartTime0@
epochStartTime0Selector :: Selector '[] (Id NSNumber)
epochStartTime0Selector = mkSelector "epochStartTime0"

-- | @Selector@ for @setEpochStartTime0:@
setEpochStartTime0Selector :: Selector '[Id NSNumber] ()
setEpochStartTime0Selector = mkSelector "setEpochStartTime0:"

-- | @Selector@ for @epochKey1@
epochKey1Selector :: Selector '[] (Id NSData)
epochKey1Selector = mkSelector "epochKey1"

-- | @Selector@ for @setEpochKey1:@
setEpochKey1Selector :: Selector '[Id NSData] ()
setEpochKey1Selector = mkSelector "setEpochKey1:"

-- | @Selector@ for @epochStartTime1@
epochStartTime1Selector :: Selector '[] (Id NSNumber)
epochStartTime1Selector = mkSelector "epochStartTime1"

-- | @Selector@ for @setEpochStartTime1:@
setEpochStartTime1Selector :: Selector '[Id NSNumber] ()
setEpochStartTime1Selector = mkSelector "setEpochStartTime1:"

-- | @Selector@ for @epochKey2@
epochKey2Selector :: Selector '[] (Id NSData)
epochKey2Selector = mkSelector "epochKey2"

-- | @Selector@ for @setEpochKey2:@
setEpochKey2Selector :: Selector '[Id NSData] ()
setEpochKey2Selector = mkSelector "setEpochKey2:"

-- | @Selector@ for @epochStartTime2@
epochStartTime2Selector :: Selector '[] (Id NSNumber)
epochStartTime2Selector = mkSelector "epochStartTime2"

-- | @Selector@ for @setEpochStartTime2:@
setEpochStartTime2Selector :: Selector '[Id NSNumber] ()
setEpochStartTime2Selector = mkSelector "setEpochStartTime2:"

-- | @Selector@ for @groupKeyMulticastPolicy@
groupKeyMulticastPolicySelector :: Selector '[] (Id NSNumber)
groupKeyMulticastPolicySelector = mkSelector "groupKeyMulticastPolicy"

-- | @Selector@ for @setGroupKeyMulticastPolicy:@
setGroupKeyMulticastPolicySelector :: Selector '[Id NSNumber] ()
setGroupKeyMulticastPolicySelector = mkSelector "setGroupKeyMulticastPolicy:"

