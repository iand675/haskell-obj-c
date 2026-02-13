{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterAppleAliroLockOperationEvent@.
module ObjC.Matter.MTRDoorLockClusterAppleAliroLockOperationEvent
  ( MTRDoorLockClusterAppleAliroLockOperationEvent
  , IsMTRDoorLockClusterAppleAliroLockOperationEvent(..)
  , lockOperationType
  , setLockOperationType
  , userIndex
  , setUserIndex
  , fabricIndex
  , setFabricIndex
  , credentials
  , setCredentials
  , credentialsSelector
  , fabricIndexSelector
  , lockOperationTypeSelector
  , setCredentialsSelector
  , setFabricIndexSelector
  , setLockOperationTypeSelector
  , setUserIndexSelector
  , userIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- lockOperationType@
lockOperationType :: IsMTRDoorLockClusterAppleAliroLockOperationEvent mtrDoorLockClusterAppleAliroLockOperationEvent => mtrDoorLockClusterAppleAliroLockOperationEvent -> IO (Id NSNumber)
lockOperationType mtrDoorLockClusterAppleAliroLockOperationEvent =
  sendMessage mtrDoorLockClusterAppleAliroLockOperationEvent lockOperationTypeSelector

-- | @- setLockOperationType:@
setLockOperationType :: (IsMTRDoorLockClusterAppleAliroLockOperationEvent mtrDoorLockClusterAppleAliroLockOperationEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockOperationEvent -> value -> IO ()
setLockOperationType mtrDoorLockClusterAppleAliroLockOperationEvent value =
  sendMessage mtrDoorLockClusterAppleAliroLockOperationEvent setLockOperationTypeSelector (toNSNumber value)

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterAppleAliroLockOperationEvent mtrDoorLockClusterAppleAliroLockOperationEvent => mtrDoorLockClusterAppleAliroLockOperationEvent -> IO (Id NSNumber)
userIndex mtrDoorLockClusterAppleAliroLockOperationEvent =
  sendMessage mtrDoorLockClusterAppleAliroLockOperationEvent userIndexSelector

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterAppleAliroLockOperationEvent mtrDoorLockClusterAppleAliroLockOperationEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockOperationEvent -> value -> IO ()
setUserIndex mtrDoorLockClusterAppleAliroLockOperationEvent value =
  sendMessage mtrDoorLockClusterAppleAliroLockOperationEvent setUserIndexSelector (toNSNumber value)

-- | @- fabricIndex@
fabricIndex :: IsMTRDoorLockClusterAppleAliroLockOperationEvent mtrDoorLockClusterAppleAliroLockOperationEvent => mtrDoorLockClusterAppleAliroLockOperationEvent -> IO (Id NSNumber)
fabricIndex mtrDoorLockClusterAppleAliroLockOperationEvent =
  sendMessage mtrDoorLockClusterAppleAliroLockOperationEvent fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRDoorLockClusterAppleAliroLockOperationEvent mtrDoorLockClusterAppleAliroLockOperationEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockOperationEvent -> value -> IO ()
setFabricIndex mtrDoorLockClusterAppleAliroLockOperationEvent value =
  sendMessage mtrDoorLockClusterAppleAliroLockOperationEvent setFabricIndexSelector (toNSNumber value)

-- | @- credentials@
credentials :: IsMTRDoorLockClusterAppleAliroLockOperationEvent mtrDoorLockClusterAppleAliroLockOperationEvent => mtrDoorLockClusterAppleAliroLockOperationEvent -> IO (Id NSArray)
credentials mtrDoorLockClusterAppleAliroLockOperationEvent =
  sendMessage mtrDoorLockClusterAppleAliroLockOperationEvent credentialsSelector

-- | @- setCredentials:@
setCredentials :: (IsMTRDoorLockClusterAppleAliroLockOperationEvent mtrDoorLockClusterAppleAliroLockOperationEvent, IsNSArray value) => mtrDoorLockClusterAppleAliroLockOperationEvent -> value -> IO ()
setCredentials mtrDoorLockClusterAppleAliroLockOperationEvent value =
  sendMessage mtrDoorLockClusterAppleAliroLockOperationEvent setCredentialsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @lockOperationType@
lockOperationTypeSelector :: Selector '[] (Id NSNumber)
lockOperationTypeSelector = mkSelector "lockOperationType"

-- | @Selector@ for @setLockOperationType:@
setLockOperationTypeSelector :: Selector '[Id NSNumber] ()
setLockOperationTypeSelector = mkSelector "setLockOperationType:"

-- | @Selector@ for @userIndex@
userIndexSelector :: Selector '[] (Id NSNumber)
userIndexSelector = mkSelector "userIndex"

-- | @Selector@ for @setUserIndex:@
setUserIndexSelector :: Selector '[Id NSNumber] ()
setUserIndexSelector = mkSelector "setUserIndex:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

-- | @Selector@ for @credentials@
credentialsSelector :: Selector '[] (Id NSArray)
credentialsSelector = mkSelector "credentials"

-- | @Selector@ for @setCredentials:@
setCredentialsSelector :: Selector '[Id NSArray] ()
setCredentialsSelector = mkSelector "setCredentials:"

