{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGroupKeyManagementClusterGroupKeySetStruct@.
module ObjC.Matter.MTRGroupKeyManagementClusterGroupKeySetStruct
  ( MTRGroupKeyManagementClusterGroupKeySetStruct
  , IsMTRGroupKeyManagementClusterGroupKeySetStruct(..)
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
  , epochKey0Selector
  , epochKey1Selector
  , epochKey2Selector
  , epochStartTime0Selector
  , epochStartTime1Selector
  , epochStartTime2Selector
  , groupKeySecurityPolicySelector
  , groupKeySetIDSelector
  , setEpochKey0Selector
  , setEpochKey1Selector
  , setEpochKey2Selector
  , setEpochStartTime0Selector
  , setEpochStartTime1Selector
  , setEpochStartTime2Selector
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
groupKeySetID :: IsMTRGroupKeyManagementClusterGroupKeySetStruct mtrGroupKeyManagementClusterGroupKeySetStruct => mtrGroupKeyManagementClusterGroupKeySetStruct -> IO (Id NSNumber)
groupKeySetID mtrGroupKeyManagementClusterGroupKeySetStruct =
  sendMessage mtrGroupKeyManagementClusterGroupKeySetStruct groupKeySetIDSelector

-- | @- setGroupKeySetID:@
setGroupKeySetID :: (IsMTRGroupKeyManagementClusterGroupKeySetStruct mtrGroupKeyManagementClusterGroupKeySetStruct, IsNSNumber value) => mtrGroupKeyManagementClusterGroupKeySetStruct -> value -> IO ()
setGroupKeySetID mtrGroupKeyManagementClusterGroupKeySetStruct value =
  sendMessage mtrGroupKeyManagementClusterGroupKeySetStruct setGroupKeySetIDSelector (toNSNumber value)

-- | @- groupKeySecurityPolicy@
groupKeySecurityPolicy :: IsMTRGroupKeyManagementClusterGroupKeySetStruct mtrGroupKeyManagementClusterGroupKeySetStruct => mtrGroupKeyManagementClusterGroupKeySetStruct -> IO (Id NSNumber)
groupKeySecurityPolicy mtrGroupKeyManagementClusterGroupKeySetStruct =
  sendMessage mtrGroupKeyManagementClusterGroupKeySetStruct groupKeySecurityPolicySelector

-- | @- setGroupKeySecurityPolicy:@
setGroupKeySecurityPolicy :: (IsMTRGroupKeyManagementClusterGroupKeySetStruct mtrGroupKeyManagementClusterGroupKeySetStruct, IsNSNumber value) => mtrGroupKeyManagementClusterGroupKeySetStruct -> value -> IO ()
setGroupKeySecurityPolicy mtrGroupKeyManagementClusterGroupKeySetStruct value =
  sendMessage mtrGroupKeyManagementClusterGroupKeySetStruct setGroupKeySecurityPolicySelector (toNSNumber value)

-- | @- epochKey0@
epochKey0 :: IsMTRGroupKeyManagementClusterGroupKeySetStruct mtrGroupKeyManagementClusterGroupKeySetStruct => mtrGroupKeyManagementClusterGroupKeySetStruct -> IO (Id NSData)
epochKey0 mtrGroupKeyManagementClusterGroupKeySetStruct =
  sendMessage mtrGroupKeyManagementClusterGroupKeySetStruct epochKey0Selector

-- | @- setEpochKey0:@
setEpochKey0 :: (IsMTRGroupKeyManagementClusterGroupKeySetStruct mtrGroupKeyManagementClusterGroupKeySetStruct, IsNSData value) => mtrGroupKeyManagementClusterGroupKeySetStruct -> value -> IO ()
setEpochKey0 mtrGroupKeyManagementClusterGroupKeySetStruct value =
  sendMessage mtrGroupKeyManagementClusterGroupKeySetStruct setEpochKey0Selector (toNSData value)

-- | @- epochStartTime0@
epochStartTime0 :: IsMTRGroupKeyManagementClusterGroupKeySetStruct mtrGroupKeyManagementClusterGroupKeySetStruct => mtrGroupKeyManagementClusterGroupKeySetStruct -> IO (Id NSNumber)
epochStartTime0 mtrGroupKeyManagementClusterGroupKeySetStruct =
  sendMessage mtrGroupKeyManagementClusterGroupKeySetStruct epochStartTime0Selector

-- | @- setEpochStartTime0:@
setEpochStartTime0 :: (IsMTRGroupKeyManagementClusterGroupKeySetStruct mtrGroupKeyManagementClusterGroupKeySetStruct, IsNSNumber value) => mtrGroupKeyManagementClusterGroupKeySetStruct -> value -> IO ()
setEpochStartTime0 mtrGroupKeyManagementClusterGroupKeySetStruct value =
  sendMessage mtrGroupKeyManagementClusterGroupKeySetStruct setEpochStartTime0Selector (toNSNumber value)

-- | @- epochKey1@
epochKey1 :: IsMTRGroupKeyManagementClusterGroupKeySetStruct mtrGroupKeyManagementClusterGroupKeySetStruct => mtrGroupKeyManagementClusterGroupKeySetStruct -> IO (Id NSData)
epochKey1 mtrGroupKeyManagementClusterGroupKeySetStruct =
  sendMessage mtrGroupKeyManagementClusterGroupKeySetStruct epochKey1Selector

-- | @- setEpochKey1:@
setEpochKey1 :: (IsMTRGroupKeyManagementClusterGroupKeySetStruct mtrGroupKeyManagementClusterGroupKeySetStruct, IsNSData value) => mtrGroupKeyManagementClusterGroupKeySetStruct -> value -> IO ()
setEpochKey1 mtrGroupKeyManagementClusterGroupKeySetStruct value =
  sendMessage mtrGroupKeyManagementClusterGroupKeySetStruct setEpochKey1Selector (toNSData value)

-- | @- epochStartTime1@
epochStartTime1 :: IsMTRGroupKeyManagementClusterGroupKeySetStruct mtrGroupKeyManagementClusterGroupKeySetStruct => mtrGroupKeyManagementClusterGroupKeySetStruct -> IO (Id NSNumber)
epochStartTime1 mtrGroupKeyManagementClusterGroupKeySetStruct =
  sendMessage mtrGroupKeyManagementClusterGroupKeySetStruct epochStartTime1Selector

-- | @- setEpochStartTime1:@
setEpochStartTime1 :: (IsMTRGroupKeyManagementClusterGroupKeySetStruct mtrGroupKeyManagementClusterGroupKeySetStruct, IsNSNumber value) => mtrGroupKeyManagementClusterGroupKeySetStruct -> value -> IO ()
setEpochStartTime1 mtrGroupKeyManagementClusterGroupKeySetStruct value =
  sendMessage mtrGroupKeyManagementClusterGroupKeySetStruct setEpochStartTime1Selector (toNSNumber value)

-- | @- epochKey2@
epochKey2 :: IsMTRGroupKeyManagementClusterGroupKeySetStruct mtrGroupKeyManagementClusterGroupKeySetStruct => mtrGroupKeyManagementClusterGroupKeySetStruct -> IO (Id NSData)
epochKey2 mtrGroupKeyManagementClusterGroupKeySetStruct =
  sendMessage mtrGroupKeyManagementClusterGroupKeySetStruct epochKey2Selector

-- | @- setEpochKey2:@
setEpochKey2 :: (IsMTRGroupKeyManagementClusterGroupKeySetStruct mtrGroupKeyManagementClusterGroupKeySetStruct, IsNSData value) => mtrGroupKeyManagementClusterGroupKeySetStruct -> value -> IO ()
setEpochKey2 mtrGroupKeyManagementClusterGroupKeySetStruct value =
  sendMessage mtrGroupKeyManagementClusterGroupKeySetStruct setEpochKey2Selector (toNSData value)

-- | @- epochStartTime2@
epochStartTime2 :: IsMTRGroupKeyManagementClusterGroupKeySetStruct mtrGroupKeyManagementClusterGroupKeySetStruct => mtrGroupKeyManagementClusterGroupKeySetStruct -> IO (Id NSNumber)
epochStartTime2 mtrGroupKeyManagementClusterGroupKeySetStruct =
  sendMessage mtrGroupKeyManagementClusterGroupKeySetStruct epochStartTime2Selector

-- | @- setEpochStartTime2:@
setEpochStartTime2 :: (IsMTRGroupKeyManagementClusterGroupKeySetStruct mtrGroupKeyManagementClusterGroupKeySetStruct, IsNSNumber value) => mtrGroupKeyManagementClusterGroupKeySetStruct -> value -> IO ()
setEpochStartTime2 mtrGroupKeyManagementClusterGroupKeySetStruct value =
  sendMessage mtrGroupKeyManagementClusterGroupKeySetStruct setEpochStartTime2Selector (toNSNumber value)

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

