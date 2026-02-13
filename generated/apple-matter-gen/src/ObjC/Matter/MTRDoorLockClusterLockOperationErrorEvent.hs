{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterLockOperationErrorEvent@.
module ObjC.Matter.MTRDoorLockClusterLockOperationErrorEvent
  ( MTRDoorLockClusterLockOperationErrorEvent
  , IsMTRDoorLockClusterLockOperationErrorEvent(..)
  , lockOperationType
  , setLockOperationType
  , operationSource
  , setOperationSource
  , operationError
  , setOperationError
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
  , operationErrorSelector
  , operationSourceSelector
  , setCredentialsSelector
  , setFabricIndexSelector
  , setLockOperationTypeSelector
  , setOperationErrorSelector
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
lockOperationType :: IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent => mtrDoorLockClusterLockOperationErrorEvent -> IO (Id NSNumber)
lockOperationType mtrDoorLockClusterLockOperationErrorEvent =
  sendMessage mtrDoorLockClusterLockOperationErrorEvent lockOperationTypeSelector

-- | @- setLockOperationType:@
setLockOperationType :: (IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent, IsNSNumber value) => mtrDoorLockClusterLockOperationErrorEvent -> value -> IO ()
setLockOperationType mtrDoorLockClusterLockOperationErrorEvent value =
  sendMessage mtrDoorLockClusterLockOperationErrorEvent setLockOperationTypeSelector (toNSNumber value)

-- | @- operationSource@
operationSource :: IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent => mtrDoorLockClusterLockOperationErrorEvent -> IO (Id NSNumber)
operationSource mtrDoorLockClusterLockOperationErrorEvent =
  sendMessage mtrDoorLockClusterLockOperationErrorEvent operationSourceSelector

-- | @- setOperationSource:@
setOperationSource :: (IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent, IsNSNumber value) => mtrDoorLockClusterLockOperationErrorEvent -> value -> IO ()
setOperationSource mtrDoorLockClusterLockOperationErrorEvent value =
  sendMessage mtrDoorLockClusterLockOperationErrorEvent setOperationSourceSelector (toNSNumber value)

-- | @- operationError@
operationError :: IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent => mtrDoorLockClusterLockOperationErrorEvent -> IO (Id NSNumber)
operationError mtrDoorLockClusterLockOperationErrorEvent =
  sendMessage mtrDoorLockClusterLockOperationErrorEvent operationErrorSelector

-- | @- setOperationError:@
setOperationError :: (IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent, IsNSNumber value) => mtrDoorLockClusterLockOperationErrorEvent -> value -> IO ()
setOperationError mtrDoorLockClusterLockOperationErrorEvent value =
  sendMessage mtrDoorLockClusterLockOperationErrorEvent setOperationErrorSelector (toNSNumber value)

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent => mtrDoorLockClusterLockOperationErrorEvent -> IO (Id NSNumber)
userIndex mtrDoorLockClusterLockOperationErrorEvent =
  sendMessage mtrDoorLockClusterLockOperationErrorEvent userIndexSelector

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent, IsNSNumber value) => mtrDoorLockClusterLockOperationErrorEvent -> value -> IO ()
setUserIndex mtrDoorLockClusterLockOperationErrorEvent value =
  sendMessage mtrDoorLockClusterLockOperationErrorEvent setUserIndexSelector (toNSNumber value)

-- | @- fabricIndex@
fabricIndex :: IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent => mtrDoorLockClusterLockOperationErrorEvent -> IO (Id NSNumber)
fabricIndex mtrDoorLockClusterLockOperationErrorEvent =
  sendMessage mtrDoorLockClusterLockOperationErrorEvent fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent, IsNSNumber value) => mtrDoorLockClusterLockOperationErrorEvent -> value -> IO ()
setFabricIndex mtrDoorLockClusterLockOperationErrorEvent value =
  sendMessage mtrDoorLockClusterLockOperationErrorEvent setFabricIndexSelector (toNSNumber value)

-- | @- sourceNode@
sourceNode :: IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent => mtrDoorLockClusterLockOperationErrorEvent -> IO (Id NSNumber)
sourceNode mtrDoorLockClusterLockOperationErrorEvent =
  sendMessage mtrDoorLockClusterLockOperationErrorEvent sourceNodeSelector

-- | @- setSourceNode:@
setSourceNode :: (IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent, IsNSNumber value) => mtrDoorLockClusterLockOperationErrorEvent -> value -> IO ()
setSourceNode mtrDoorLockClusterLockOperationErrorEvent value =
  sendMessage mtrDoorLockClusterLockOperationErrorEvent setSourceNodeSelector (toNSNumber value)

-- | @- credentials@
credentials :: IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent => mtrDoorLockClusterLockOperationErrorEvent -> IO (Id NSArray)
credentials mtrDoorLockClusterLockOperationErrorEvent =
  sendMessage mtrDoorLockClusterLockOperationErrorEvent credentialsSelector

-- | @- setCredentials:@
setCredentials :: (IsMTRDoorLockClusterLockOperationErrorEvent mtrDoorLockClusterLockOperationErrorEvent, IsNSArray value) => mtrDoorLockClusterLockOperationErrorEvent -> value -> IO ()
setCredentials mtrDoorLockClusterLockOperationErrorEvent value =
  sendMessage mtrDoorLockClusterLockOperationErrorEvent setCredentialsSelector (toNSArray value)

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

-- | @Selector@ for @operationError@
operationErrorSelector :: Selector '[] (Id NSNumber)
operationErrorSelector = mkSelector "operationError"

-- | @Selector@ for @setOperationError:@
setOperationErrorSelector :: Selector '[Id NSNumber] ()
setOperationErrorSelector = mkSelector "setOperationError:"

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

