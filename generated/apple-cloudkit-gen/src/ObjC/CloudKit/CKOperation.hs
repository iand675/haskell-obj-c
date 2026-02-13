{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKOperation@.
module ObjC.CloudKit.CKOperation
  ( CKOperation
  , IsCKOperation(..)
  , init_
  , configuration
  , setConfiguration
  , group
  , setGroup
  , operationID
  , longLivedOperationWasPersistedBlock
  , setLongLivedOperationWasPersistedBlock
  , container
  , setContainer
  , allowsCellularAccess
  , setAllowsCellularAccess
  , longLived
  , setLongLived
  , timeoutIntervalForRequest
  , setTimeoutIntervalForRequest
  , timeoutIntervalForResource
  , setTimeoutIntervalForResource
  , allowsCellularAccessSelector
  , configurationSelector
  , containerSelector
  , groupSelector
  , initSelector
  , longLivedOperationWasPersistedBlockSelector
  , longLivedSelector
  , operationIDSelector
  , setAllowsCellularAccessSelector
  , setConfigurationSelector
  , setContainerSelector
  , setGroupSelector
  , setLongLivedOperationWasPersistedBlockSelector
  , setLongLivedSelector
  , setTimeoutIntervalForRequestSelector
  , setTimeoutIntervalForResourceSelector
  , timeoutIntervalForRequestSelector
  , timeoutIntervalForResourceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKOperation ckOperation => ckOperation -> IO (Id CKOperation)
init_ ckOperation =
  sendOwnedMessage ckOperation initSelector

-- | This defines per-operation configuration settings.
--
-- See the CKOperationConfiguration class description for info on how this configuration composes with CKOperationGroup.defaultConfiguration
--
-- ObjC selector: @- configuration@
configuration :: IsCKOperation ckOperation => ckOperation -> IO (Id CKOperationConfiguration)
configuration ckOperation =
  sendMessage ckOperation configurationSelector

-- | This defines per-operation configuration settings.
--
-- See the CKOperationConfiguration class description for info on how this configuration composes with CKOperationGroup.defaultConfiguration
--
-- ObjC selector: @- setConfiguration:@
setConfiguration :: (IsCKOperation ckOperation, IsCKOperationConfiguration value) => ckOperation -> value -> IO ()
setConfiguration ckOperation value =
  sendMessage ckOperation setConfigurationSelector (toCKOperationConfiguration value)

-- | The group this operation is associated with
--
-- ObjC selector: @- group@
group :: IsCKOperation ckOperation => ckOperation -> IO (Id CKOperationGroup)
group ckOperation =
  sendMessage ckOperation groupSelector

-- | The group this operation is associated with
--
-- ObjC selector: @- setGroup:@
setGroup :: (IsCKOperation ckOperation, IsCKOperationGroup value) => ckOperation -> value -> IO ()
setGroup ckOperation value =
  sendMessage ckOperation setGroupSelector (toCKOperationGroup value)

-- | This is an identifier unique to this CKOperation.
--
-- This value is chosen by the system, and will be unique to this instance of a CKOperation.  This identifier will be sent to Apple's servers, and can be used to identify any server-side logging associated with this operation.
--
-- ObjC selector: @- operationID@
operationID :: IsCKOperation ckOperation => ckOperation -> IO (Id NSString)
operationID ckOperation =
  sendMessage ckOperation operationIDSelector

-- | This callback is called after a long lived operation has begun running and is persisted.
--
-- Once this callback is called the operation will continue running even if the current process exits.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- longLivedOperationWasPersistedBlock@
longLivedOperationWasPersistedBlock :: IsCKOperation ckOperation => ckOperation -> IO (Ptr ())
longLivedOperationWasPersistedBlock ckOperation =
  sendMessage ckOperation longLivedOperationWasPersistedBlockSelector

-- | This callback is called after a long lived operation has begun running and is persisted.
--
-- Once this callback is called the operation will continue running even if the current process exits.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setLongLivedOperationWasPersistedBlock:@
setLongLivedOperationWasPersistedBlock :: IsCKOperation ckOperation => ckOperation -> Ptr () -> IO ()
setLongLivedOperationWasPersistedBlock ckOperation value =
  sendMessage ckOperation setLongLivedOperationWasPersistedBlockSelector value

-- | @- container@
container :: IsCKOperation ckOperation => ckOperation -> IO (Id CKContainer)
container ckOperation =
  sendMessage ckOperation containerSelector

-- | @- setContainer:@
setContainer :: (IsCKOperation ckOperation, IsCKContainer value) => ckOperation -> value -> IO ()
setContainer ckOperation value =
  sendMessage ckOperation setContainerSelector (toCKContainer value)

-- | @- allowsCellularAccess@
allowsCellularAccess :: IsCKOperation ckOperation => ckOperation -> IO Bool
allowsCellularAccess ckOperation =
  sendMessage ckOperation allowsCellularAccessSelector

-- | @- setAllowsCellularAccess:@
setAllowsCellularAccess :: IsCKOperation ckOperation => ckOperation -> Bool -> IO ()
setAllowsCellularAccess ckOperation value =
  sendMessage ckOperation setAllowsCellularAccessSelector value

-- | @- longLived@
longLived :: IsCKOperation ckOperation => ckOperation -> IO Bool
longLived ckOperation =
  sendMessage ckOperation longLivedSelector

-- | @- setLongLived:@
setLongLived :: IsCKOperation ckOperation => ckOperation -> Bool -> IO ()
setLongLived ckOperation value =
  sendMessage ckOperation setLongLivedSelector value

-- | @- timeoutIntervalForRequest@
timeoutIntervalForRequest :: IsCKOperation ckOperation => ckOperation -> IO CDouble
timeoutIntervalForRequest ckOperation =
  sendMessage ckOperation timeoutIntervalForRequestSelector

-- | @- setTimeoutIntervalForRequest:@
setTimeoutIntervalForRequest :: IsCKOperation ckOperation => ckOperation -> CDouble -> IO ()
setTimeoutIntervalForRequest ckOperation value =
  sendMessage ckOperation setTimeoutIntervalForRequestSelector value

-- | @- timeoutIntervalForResource@
timeoutIntervalForResource :: IsCKOperation ckOperation => ckOperation -> IO CDouble
timeoutIntervalForResource ckOperation =
  sendMessage ckOperation timeoutIntervalForResourceSelector

-- | @- setTimeoutIntervalForResource:@
setTimeoutIntervalForResource :: IsCKOperation ckOperation => ckOperation -> CDouble -> IO ()
setTimeoutIntervalForResource ckOperation value =
  sendMessage ckOperation setTimeoutIntervalForResourceSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKOperation)
initSelector = mkSelector "init"

-- | @Selector@ for @configuration@
configurationSelector :: Selector '[] (Id CKOperationConfiguration)
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @setConfiguration:@
setConfigurationSelector :: Selector '[Id CKOperationConfiguration] ()
setConfigurationSelector = mkSelector "setConfiguration:"

-- | @Selector@ for @group@
groupSelector :: Selector '[] (Id CKOperationGroup)
groupSelector = mkSelector "group"

-- | @Selector@ for @setGroup:@
setGroupSelector :: Selector '[Id CKOperationGroup] ()
setGroupSelector = mkSelector "setGroup:"

-- | @Selector@ for @operationID@
operationIDSelector :: Selector '[] (Id NSString)
operationIDSelector = mkSelector "operationID"

-- | @Selector@ for @longLivedOperationWasPersistedBlock@
longLivedOperationWasPersistedBlockSelector :: Selector '[] (Ptr ())
longLivedOperationWasPersistedBlockSelector = mkSelector "longLivedOperationWasPersistedBlock"

-- | @Selector@ for @setLongLivedOperationWasPersistedBlock:@
setLongLivedOperationWasPersistedBlockSelector :: Selector '[Ptr ()] ()
setLongLivedOperationWasPersistedBlockSelector = mkSelector "setLongLivedOperationWasPersistedBlock:"

-- | @Selector@ for @container@
containerSelector :: Selector '[] (Id CKContainer)
containerSelector = mkSelector "container"

-- | @Selector@ for @setContainer:@
setContainerSelector :: Selector '[Id CKContainer] ()
setContainerSelector = mkSelector "setContainer:"

-- | @Selector@ for @allowsCellularAccess@
allowsCellularAccessSelector :: Selector '[] Bool
allowsCellularAccessSelector = mkSelector "allowsCellularAccess"

-- | @Selector@ for @setAllowsCellularAccess:@
setAllowsCellularAccessSelector :: Selector '[Bool] ()
setAllowsCellularAccessSelector = mkSelector "setAllowsCellularAccess:"

-- | @Selector@ for @longLived@
longLivedSelector :: Selector '[] Bool
longLivedSelector = mkSelector "longLived"

-- | @Selector@ for @setLongLived:@
setLongLivedSelector :: Selector '[Bool] ()
setLongLivedSelector = mkSelector "setLongLived:"

-- | @Selector@ for @timeoutIntervalForRequest@
timeoutIntervalForRequestSelector :: Selector '[] CDouble
timeoutIntervalForRequestSelector = mkSelector "timeoutIntervalForRequest"

-- | @Selector@ for @setTimeoutIntervalForRequest:@
setTimeoutIntervalForRequestSelector :: Selector '[CDouble] ()
setTimeoutIntervalForRequestSelector = mkSelector "setTimeoutIntervalForRequest:"

-- | @Selector@ for @timeoutIntervalForResource@
timeoutIntervalForResourceSelector :: Selector '[] CDouble
timeoutIntervalForResourceSelector = mkSelector "timeoutIntervalForResource"

-- | @Selector@ for @setTimeoutIntervalForResource:@
setTimeoutIntervalForResourceSelector :: Selector '[CDouble] ()
setTimeoutIntervalForResourceSelector = mkSelector "setTimeoutIntervalForResource:"

