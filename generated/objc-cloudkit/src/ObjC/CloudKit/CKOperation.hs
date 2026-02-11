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
  , initSelector
  , configurationSelector
  , setConfigurationSelector
  , groupSelector
  , setGroupSelector
  , operationIDSelector
  , longLivedOperationWasPersistedBlockSelector
  , setLongLivedOperationWasPersistedBlockSelector
  , containerSelector
  , setContainerSelector
  , allowsCellularAccessSelector
  , setAllowsCellularAccessSelector
  , longLivedSelector
  , setLongLivedSelector
  , timeoutIntervalForRequestSelector
  , setTimeoutIntervalForRequestSelector
  , timeoutIntervalForResourceSelector
  , setTimeoutIntervalForResourceSelector


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

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKOperation ckOperation => ckOperation -> IO (Id CKOperation)
init_ ckOperation  =
  sendMsg ckOperation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | This defines per-operation configuration settings.
--
-- See the CKOperationConfiguration class description for info on how this configuration composes with CKOperationGroup.defaultConfiguration
--
-- ObjC selector: @- configuration@
configuration :: IsCKOperation ckOperation => ckOperation -> IO (Id CKOperationConfiguration)
configuration ckOperation  =
  sendMsg ckOperation (mkSelector "configuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | This defines per-operation configuration settings.
--
-- See the CKOperationConfiguration class description for info on how this configuration composes with CKOperationGroup.defaultConfiguration
--
-- ObjC selector: @- setConfiguration:@
setConfiguration :: (IsCKOperation ckOperation, IsCKOperationConfiguration value) => ckOperation -> value -> IO ()
setConfiguration ckOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckOperation (mkSelector "setConfiguration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The group this operation is associated with
--
-- ObjC selector: @- group@
group :: IsCKOperation ckOperation => ckOperation -> IO (Id CKOperationGroup)
group ckOperation  =
  sendMsg ckOperation (mkSelector "group") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The group this operation is associated with
--
-- ObjC selector: @- setGroup:@
setGroup :: (IsCKOperation ckOperation, IsCKOperationGroup value) => ckOperation -> value -> IO ()
setGroup ckOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckOperation (mkSelector "setGroup:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | This is an identifier unique to this CKOperation.
--
-- This value is chosen by the system, and will be unique to this instance of a CKOperation.  This identifier will be sent to Apple's servers, and can be used to identify any server-side logging associated with this operation.
--
-- ObjC selector: @- operationID@
operationID :: IsCKOperation ckOperation => ckOperation -> IO (Id NSString)
operationID ckOperation  =
  sendMsg ckOperation (mkSelector "operationID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | This callback is called after a long lived operation has begun running and is persisted.
--
-- Once this callback is called the operation will continue running even if the current process exits.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- longLivedOperationWasPersistedBlock@
longLivedOperationWasPersistedBlock :: IsCKOperation ckOperation => ckOperation -> IO (Ptr ())
longLivedOperationWasPersistedBlock ckOperation  =
  fmap castPtr $ sendMsg ckOperation (mkSelector "longLivedOperationWasPersistedBlock") (retPtr retVoid) []

-- | This callback is called after a long lived operation has begun running and is persisted.
--
-- Once this callback is called the operation will continue running even if the current process exits.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setLongLivedOperationWasPersistedBlock:@
setLongLivedOperationWasPersistedBlock :: IsCKOperation ckOperation => ckOperation -> Ptr () -> IO ()
setLongLivedOperationWasPersistedBlock ckOperation  value =
  sendMsg ckOperation (mkSelector "setLongLivedOperationWasPersistedBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- container@
container :: IsCKOperation ckOperation => ckOperation -> IO (Id CKContainer)
container ckOperation  =
  sendMsg ckOperation (mkSelector "container") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContainer:@
setContainer :: (IsCKOperation ckOperation, IsCKContainer value) => ckOperation -> value -> IO ()
setContainer ckOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckOperation (mkSelector "setContainer:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- allowsCellularAccess@
allowsCellularAccess :: IsCKOperation ckOperation => ckOperation -> IO Bool
allowsCellularAccess ckOperation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ckOperation (mkSelector "allowsCellularAccess") retCULong []

-- | @- setAllowsCellularAccess:@
setAllowsCellularAccess :: IsCKOperation ckOperation => ckOperation -> Bool -> IO ()
setAllowsCellularAccess ckOperation  value =
  sendMsg ckOperation (mkSelector "setAllowsCellularAccess:") retVoid [argCULong (if value then 1 else 0)]

-- | @- longLived@
longLived :: IsCKOperation ckOperation => ckOperation -> IO Bool
longLived ckOperation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ckOperation (mkSelector "longLived") retCULong []

-- | @- setLongLived:@
setLongLived :: IsCKOperation ckOperation => ckOperation -> Bool -> IO ()
setLongLived ckOperation  value =
  sendMsg ckOperation (mkSelector "setLongLived:") retVoid [argCULong (if value then 1 else 0)]

-- | @- timeoutIntervalForRequest@
timeoutIntervalForRequest :: IsCKOperation ckOperation => ckOperation -> IO CDouble
timeoutIntervalForRequest ckOperation  =
  sendMsg ckOperation (mkSelector "timeoutIntervalForRequest") retCDouble []

-- | @- setTimeoutIntervalForRequest:@
setTimeoutIntervalForRequest :: IsCKOperation ckOperation => ckOperation -> CDouble -> IO ()
setTimeoutIntervalForRequest ckOperation  value =
  sendMsg ckOperation (mkSelector "setTimeoutIntervalForRequest:") retVoid [argCDouble (fromIntegral value)]

-- | @- timeoutIntervalForResource@
timeoutIntervalForResource :: IsCKOperation ckOperation => ckOperation -> IO CDouble
timeoutIntervalForResource ckOperation  =
  sendMsg ckOperation (mkSelector "timeoutIntervalForResource") retCDouble []

-- | @- setTimeoutIntervalForResource:@
setTimeoutIntervalForResource :: IsCKOperation ckOperation => ckOperation -> CDouble -> IO ()
setTimeoutIntervalForResource ckOperation  value =
  sendMsg ckOperation (mkSelector "setTimeoutIntervalForResource:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @configuration@
configurationSelector :: Selector
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @setConfiguration:@
setConfigurationSelector :: Selector
setConfigurationSelector = mkSelector "setConfiguration:"

-- | @Selector@ for @group@
groupSelector :: Selector
groupSelector = mkSelector "group"

-- | @Selector@ for @setGroup:@
setGroupSelector :: Selector
setGroupSelector = mkSelector "setGroup:"

-- | @Selector@ for @operationID@
operationIDSelector :: Selector
operationIDSelector = mkSelector "operationID"

-- | @Selector@ for @longLivedOperationWasPersistedBlock@
longLivedOperationWasPersistedBlockSelector :: Selector
longLivedOperationWasPersistedBlockSelector = mkSelector "longLivedOperationWasPersistedBlock"

-- | @Selector@ for @setLongLivedOperationWasPersistedBlock:@
setLongLivedOperationWasPersistedBlockSelector :: Selector
setLongLivedOperationWasPersistedBlockSelector = mkSelector "setLongLivedOperationWasPersistedBlock:"

-- | @Selector@ for @container@
containerSelector :: Selector
containerSelector = mkSelector "container"

-- | @Selector@ for @setContainer:@
setContainerSelector :: Selector
setContainerSelector = mkSelector "setContainer:"

-- | @Selector@ for @allowsCellularAccess@
allowsCellularAccessSelector :: Selector
allowsCellularAccessSelector = mkSelector "allowsCellularAccess"

-- | @Selector@ for @setAllowsCellularAccess:@
setAllowsCellularAccessSelector :: Selector
setAllowsCellularAccessSelector = mkSelector "setAllowsCellularAccess:"

-- | @Selector@ for @longLived@
longLivedSelector :: Selector
longLivedSelector = mkSelector "longLived"

-- | @Selector@ for @setLongLived:@
setLongLivedSelector :: Selector
setLongLivedSelector = mkSelector "setLongLived:"

-- | @Selector@ for @timeoutIntervalForRequest@
timeoutIntervalForRequestSelector :: Selector
timeoutIntervalForRequestSelector = mkSelector "timeoutIntervalForRequest"

-- | @Selector@ for @setTimeoutIntervalForRequest:@
setTimeoutIntervalForRequestSelector :: Selector
setTimeoutIntervalForRequestSelector = mkSelector "setTimeoutIntervalForRequest:"

-- | @Selector@ for @timeoutIntervalForResource@
timeoutIntervalForResourceSelector :: Selector
timeoutIntervalForResourceSelector = mkSelector "timeoutIntervalForResource"

-- | @Selector@ for @setTimeoutIntervalForResource:@
setTimeoutIntervalForResourceSelector :: Selector
setTimeoutIntervalForResourceSelector = mkSelector "setTimeoutIntervalForResource:"

