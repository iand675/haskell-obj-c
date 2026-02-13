{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterLockOperationEvent@.
module ObjC.Matter.MTRDoorLockClusterLockOperationEvent
  ( MTRDoorLockClusterLockOperationEvent
  , IsMTRDoorLockClusterLockOperationEvent(..)
  , lockOperationType
  , setLockOperationType
  , operationSource
  , setOperationSource
  , userIndex
  , setUserIndex
  , fabricIndex
  , setFabricIndex
  , sourceNode
  , setSourceNode
  , credentials
  , setCredentials
  , credentialsSelector
  , fabricIndexSelector
  , lockOperationTypeSelector
  , operationSourceSelector
  , setCredentialsSelector
  , setFabricIndexSelector
  , setLockOperationTypeSelector
  , setOperationSourceSelector
  , setSourceNodeSelector
  , setUserIndexSelector
  , sourceNodeSelector
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
lockOperationType :: IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent => mtrDoorLockClusterLockOperationEvent -> IO (Id NSNumber)
lockOperationType mtrDoorLockClusterLockOperationEvent =
  sendMessage mtrDoorLockClusterLockOperationEvent lockOperationTypeSelector

-- | @- setLockOperationType:@
setLockOperationType :: (IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent, IsNSNumber value) => mtrDoorLockClusterLockOperationEvent -> value -> IO ()
setLockOperationType mtrDoorLockClusterLockOperationEvent value =
  sendMessage mtrDoorLockClusterLockOperationEvent setLockOperationTypeSelector (toNSNumber value)

-- | @- operationSource@
operationSource :: IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent => mtrDoorLockClusterLockOperationEvent -> IO (Id NSNumber)
operationSource mtrDoorLockClusterLockOperationEvent =
  sendMessage mtrDoorLockClusterLockOperationEvent operationSourceSelector

-- | @- setOperationSource:@
setOperationSource :: (IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent, IsNSNumber value) => mtrDoorLockClusterLockOperationEvent -> value -> IO ()
setOperationSource mtrDoorLockClusterLockOperationEvent value =
  sendMessage mtrDoorLockClusterLockOperationEvent setOperationSourceSelector (toNSNumber value)

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent => mtrDoorLockClusterLockOperationEvent -> IO (Id NSNumber)
userIndex mtrDoorLockClusterLockOperationEvent =
  sendMessage mtrDoorLockClusterLockOperationEvent userIndexSelector

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent, IsNSNumber value) => mtrDoorLockClusterLockOperationEvent -> value -> IO ()
setUserIndex mtrDoorLockClusterLockOperationEvent value =
  sendMessage mtrDoorLockClusterLockOperationEvent setUserIndexSelector (toNSNumber value)

-- | @- fabricIndex@
fabricIndex :: IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent => mtrDoorLockClusterLockOperationEvent -> IO (Id NSNumber)
fabricIndex mtrDoorLockClusterLockOperationEvent =
  sendMessage mtrDoorLockClusterLockOperationEvent fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent, IsNSNumber value) => mtrDoorLockClusterLockOperationEvent -> value -> IO ()
setFabricIndex mtrDoorLockClusterLockOperationEvent value =
  sendMessage mtrDoorLockClusterLockOperationEvent setFabricIndexSelector (toNSNumber value)

-- | @- sourceNode@
sourceNode :: IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent => mtrDoorLockClusterLockOperationEvent -> IO (Id NSNumber)
sourceNode mtrDoorLockClusterLockOperationEvent =
  sendMessage mtrDoorLockClusterLockOperationEvent sourceNodeSelector

-- | @- setSourceNode:@
setSourceNode :: (IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent, IsNSNumber value) => mtrDoorLockClusterLockOperationEvent -> value -> IO ()
setSourceNode mtrDoorLockClusterLockOperationEvent value =
  sendMessage mtrDoorLockClusterLockOperationEvent setSourceNodeSelector (toNSNumber value)

-- | @- credentials@
credentials :: IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent => mtrDoorLockClusterLockOperationEvent -> IO (Id NSArray)
credentials mtrDoorLockClusterLockOperationEvent =
  sendMessage mtrDoorLockClusterLockOperationEvent credentialsSelector

-- | @- setCredentials:@
setCredentials :: (IsMTRDoorLockClusterLockOperationEvent mtrDoorLockClusterLockOperationEvent, IsNSArray value) => mtrDoorLockClusterLockOperationEvent -> value -> IO ()
setCredentials mtrDoorLockClusterLockOperationEvent value =
  sendMessage mtrDoorLockClusterLockOperationEvent setCredentialsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @lockOperationType@
lockOperationTypeSelector :: Selector '[] (Id NSNumber)
lockOperationTypeSelector = mkSelector "lockOperationType"

-- | @Selector@ for @setLockOperationType:@
setLockOperationTypeSelector :: Selector '[Id NSNumber] ()
setLockOperationTypeSelector = mkSelector "setLockOperationType:"

-- | @Selector@ for @operationSource@
operationSourceSelector :: Selector '[] (Id NSNumber)
operationSourceSelector = mkSelector "operationSource"

-- | @Selector@ for @setOperationSource:@
setOperationSourceSelector :: Selector '[Id NSNumber] ()
setOperationSourceSelector = mkSelector "setOperationSource:"

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

-- | @Selector@ for @sourceNode@
sourceNodeSelector :: Selector '[] (Id NSNumber)
sourceNodeSelector = mkSelector "sourceNode"

-- | @Selector@ for @setSourceNode:@
setSourceNodeSelector :: Selector '[Id NSNumber] ()
setSourceNodeSelector = mkSelector "setSourceNode:"

-- | @Selector@ for @credentials@
credentialsSelector :: Selector '[] (Id NSArray)
credentialsSelector = mkSelector "credentials"

-- | @Selector@ for @setCredentials:@
setCredentialsSelector :: Selector '[Id NSArray] ()
setCredentialsSelector = mkSelector "setCredentials:"

