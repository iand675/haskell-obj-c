{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDoorLockClusterAppleAliroLockOperationErrorEvent@.
module ObjC.Matter.MTRDoorLockClusterAppleAliroLockOperationErrorEvent
  ( MTRDoorLockClusterAppleAliroLockOperationErrorEvent
  , IsMTRDoorLockClusterAppleAliroLockOperationErrorEvent(..)
  , lockOperationType
  , setLockOperationType
  , operationError
  , setOperationError
  , userIndex
  , setUserIndex
  , fabricIndex
  , setFabricIndex
  , credentials
  , setCredentials
  , credentialsSelector
  , fabricIndexSelector
  , lockOperationTypeSelector
  , operationErrorSelector
  , setCredentialsSelector
  , setFabricIndexSelector
  , setLockOperationTypeSelector
  , setOperationErrorSelector
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
lockOperationType :: IsMTRDoorLockClusterAppleAliroLockOperationErrorEvent mtrDoorLockClusterAppleAliroLockOperationErrorEvent => mtrDoorLockClusterAppleAliroLockOperationErrorEvent -> IO (Id NSNumber)
lockOperationType mtrDoorLockClusterAppleAliroLockOperationErrorEvent =
  sendMessage mtrDoorLockClusterAppleAliroLockOperationErrorEvent lockOperationTypeSelector

-- | @- setLockOperationType:@
setLockOperationType :: (IsMTRDoorLockClusterAppleAliroLockOperationErrorEvent mtrDoorLockClusterAppleAliroLockOperationErrorEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockOperationErrorEvent -> value -> IO ()
setLockOperationType mtrDoorLockClusterAppleAliroLockOperationErrorEvent value =
  sendMessage mtrDoorLockClusterAppleAliroLockOperationErrorEvent setLockOperationTypeSelector (toNSNumber value)

-- | @- operationError@
operationError :: IsMTRDoorLockClusterAppleAliroLockOperationErrorEvent mtrDoorLockClusterAppleAliroLockOperationErrorEvent => mtrDoorLockClusterAppleAliroLockOperationErrorEvent -> IO (Id NSNumber)
operationError mtrDoorLockClusterAppleAliroLockOperationErrorEvent =
  sendMessage mtrDoorLockClusterAppleAliroLockOperationErrorEvent operationErrorSelector

-- | @- setOperationError:@
setOperationError :: (IsMTRDoorLockClusterAppleAliroLockOperationErrorEvent mtrDoorLockClusterAppleAliroLockOperationErrorEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockOperationErrorEvent -> value -> IO ()
setOperationError mtrDoorLockClusterAppleAliroLockOperationErrorEvent value =
  sendMessage mtrDoorLockClusterAppleAliroLockOperationErrorEvent setOperationErrorSelector (toNSNumber value)

-- | @- userIndex@
userIndex :: IsMTRDoorLockClusterAppleAliroLockOperationErrorEvent mtrDoorLockClusterAppleAliroLockOperationErrorEvent => mtrDoorLockClusterAppleAliroLockOperationErrorEvent -> IO (Id NSNumber)
userIndex mtrDoorLockClusterAppleAliroLockOperationErrorEvent =
  sendMessage mtrDoorLockClusterAppleAliroLockOperationErrorEvent userIndexSelector

-- | @- setUserIndex:@
setUserIndex :: (IsMTRDoorLockClusterAppleAliroLockOperationErrorEvent mtrDoorLockClusterAppleAliroLockOperationErrorEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockOperationErrorEvent -> value -> IO ()
setUserIndex mtrDoorLockClusterAppleAliroLockOperationErrorEvent value =
  sendMessage mtrDoorLockClusterAppleAliroLockOperationErrorEvent setUserIndexSelector (toNSNumber value)

-- | @- fabricIndex@
fabricIndex :: IsMTRDoorLockClusterAppleAliroLockOperationErrorEvent mtrDoorLockClusterAppleAliroLockOperationErrorEvent => mtrDoorLockClusterAppleAliroLockOperationErrorEvent -> IO (Id NSNumber)
fabricIndex mtrDoorLockClusterAppleAliroLockOperationErrorEvent =
  sendMessage mtrDoorLockClusterAppleAliroLockOperationErrorEvent fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRDoorLockClusterAppleAliroLockOperationErrorEvent mtrDoorLockClusterAppleAliroLockOperationErrorEvent, IsNSNumber value) => mtrDoorLockClusterAppleAliroLockOperationErrorEvent -> value -> IO ()
setFabricIndex mtrDoorLockClusterAppleAliroLockOperationErrorEvent value =
  sendMessage mtrDoorLockClusterAppleAliroLockOperationErrorEvent setFabricIndexSelector (toNSNumber value)

-- | @- credentials@
credentials :: IsMTRDoorLockClusterAppleAliroLockOperationErrorEvent mtrDoorLockClusterAppleAliroLockOperationErrorEvent => mtrDoorLockClusterAppleAliroLockOperationErrorEvent -> IO (Id NSArray)
credentials mtrDoorLockClusterAppleAliroLockOperationErrorEvent =
  sendMessage mtrDoorLockClusterAppleAliroLockOperationErrorEvent credentialsSelector

-- | @- setCredentials:@
setCredentials :: (IsMTRDoorLockClusterAppleAliroLockOperationErrorEvent mtrDoorLockClusterAppleAliroLockOperationErrorEvent, IsNSArray value) => mtrDoorLockClusterAppleAliroLockOperationErrorEvent -> value -> IO ()
setCredentials mtrDoorLockClusterAppleAliroLockOperationErrorEvent value =
  sendMessage mtrDoorLockClusterAppleAliroLockOperationErrorEvent setCredentialsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @lockOperationType@
lockOperationTypeSelector :: Selector '[] (Id NSNumber)
lockOperationTypeSelector = mkSelector "lockOperationType"

-- | @Selector@ for @setLockOperationType:@
setLockOperationTypeSelector :: Selector '[Id NSNumber] ()
setLockOperationTypeSelector = mkSelector "setLockOperationType:"

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

-- | @Selector@ for @credentials@
credentialsSelector :: Selector '[] (Id NSArray)
credentialsSelector = mkSelector "credentials"

-- | @Selector@ for @setCredentials:@
setCredentialsSelector :: Selector '[Id NSArray] ()
setCredentialsSelector = mkSelector "setCredentials:"

