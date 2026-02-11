{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A set of options to use when fetching changes from the server.
--
-- Generated bindings for @CKSyncEngineFetchChangesOptions@.
module ObjC.CloudKit.CKSyncEngineFetchChangesOptions
  ( CKSyncEngineFetchChangesOptions
  , IsCKSyncEngineFetchChangesOptions(..)
  , initWithScope
  , scope
  , setScope
  , operationGroup
  , setOperationGroup
  , prioritizedZoneIDs
  , setPrioritizedZoneIDs
  , initWithScopeSelector
  , scopeSelector
  , setScopeSelector
  , operationGroupSelector
  , setOperationGroupSelector
  , prioritizedZoneIDsSelector
  , setPrioritizedZoneIDsSelector


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

-- | Initializes a set of options with the specific scope. If no scope is provided, the default scope will include everything.
--
-- ObjC selector: @- initWithScope:@
initWithScope :: (IsCKSyncEngineFetchChangesOptions ckSyncEngineFetchChangesOptions, IsCKSyncEngineFetchChangesScope scope) => ckSyncEngineFetchChangesOptions -> scope -> IO (Id CKSyncEngineFetchChangesOptions)
initWithScope ckSyncEngineFetchChangesOptions  scope =
withObjCPtr scope $ \raw_scope ->
    sendMsg ckSyncEngineFetchChangesOptions (mkSelector "initWithScope:") (retPtr retVoid) [argPtr (castPtr raw_scope :: Ptr ())] >>= ownedObject . castPtr

-- | The scope in which to fetch changes from the server.
--
-- ObjC selector: @- scope@
scope :: IsCKSyncEngineFetchChangesOptions ckSyncEngineFetchChangesOptions => ckSyncEngineFetchChangesOptions -> IO (Id CKSyncEngineFetchChangesScope)
scope ckSyncEngineFetchChangesOptions  =
  sendMsg ckSyncEngineFetchChangesOptions (mkSelector "scope") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The scope in which to fetch changes from the server.
--
-- ObjC selector: @- setScope:@
setScope :: (IsCKSyncEngineFetchChangesOptions ckSyncEngineFetchChangesOptions, IsCKSyncEngineFetchChangesScope value) => ckSyncEngineFetchChangesOptions -> value -> IO ()
setScope ckSyncEngineFetchChangesOptions  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckSyncEngineFetchChangesOptions (mkSelector "setScope:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The operation group to use for the underlying operations when fetching changes.
--
-- You might set an operation group with a particular name in order to help you analyze telemetry in the CloudKit Console. If you don't provide an operation group, a default one will be created for you.
--
-- ObjC selector: @- operationGroup@
operationGroup :: IsCKSyncEngineFetchChangesOptions ckSyncEngineFetchChangesOptions => ckSyncEngineFetchChangesOptions -> IO (Id CKOperationGroup)
operationGroup ckSyncEngineFetchChangesOptions  =
  sendMsg ckSyncEngineFetchChangesOptions (mkSelector "operationGroup") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The operation group to use for the underlying operations when fetching changes.
--
-- You might set an operation group with a particular name in order to help you analyze telemetry in the CloudKit Console. If you don't provide an operation group, a default one will be created for you.
--
-- ObjC selector: @- setOperationGroup:@
setOperationGroup :: (IsCKSyncEngineFetchChangesOptions ckSyncEngineFetchChangesOptions, IsCKOperationGroup value) => ckSyncEngineFetchChangesOptions -> value -> IO ()
setOperationGroup ckSyncEngineFetchChangesOptions  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckSyncEngineFetchChangesOptions (mkSelector "setOperationGroup:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A list of zones that should be prioritized over others while fetching changes.
--
-- @CKSyncEngine@ will fetch changes for the zones in this list first before any other zones. You might use this to prioritize a specific set of zones for initial sync. You could also prioritize the object currently showing in the UI by putting it first in this list.
--
-- Any zones not included in this list will be prioritized in a default manner. If a zone in this list has no changes to fetch, then that zone will be ignored.
--
-- ObjC selector: @- prioritizedZoneIDs@
prioritizedZoneIDs :: IsCKSyncEngineFetchChangesOptions ckSyncEngineFetchChangesOptions => ckSyncEngineFetchChangesOptions -> IO (Id NSArray)
prioritizedZoneIDs ckSyncEngineFetchChangesOptions  =
  sendMsg ckSyncEngineFetchChangesOptions (mkSelector "prioritizedZoneIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A list of zones that should be prioritized over others while fetching changes.
--
-- @CKSyncEngine@ will fetch changes for the zones in this list first before any other zones. You might use this to prioritize a specific set of zones for initial sync. You could also prioritize the object currently showing in the UI by putting it first in this list.
--
-- Any zones not included in this list will be prioritized in a default manner. If a zone in this list has no changes to fetch, then that zone will be ignored.
--
-- ObjC selector: @- setPrioritizedZoneIDs:@
setPrioritizedZoneIDs :: (IsCKSyncEngineFetchChangesOptions ckSyncEngineFetchChangesOptions, IsNSArray value) => ckSyncEngineFetchChangesOptions -> value -> IO ()
setPrioritizedZoneIDs ckSyncEngineFetchChangesOptions  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckSyncEngineFetchChangesOptions (mkSelector "setPrioritizedZoneIDs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithScope:@
initWithScopeSelector :: Selector
initWithScopeSelector = mkSelector "initWithScope:"

-- | @Selector@ for @scope@
scopeSelector :: Selector
scopeSelector = mkSelector "scope"

-- | @Selector@ for @setScope:@
setScopeSelector :: Selector
setScopeSelector = mkSelector "setScope:"

-- | @Selector@ for @operationGroup@
operationGroupSelector :: Selector
operationGroupSelector = mkSelector "operationGroup"

-- | @Selector@ for @setOperationGroup:@
setOperationGroupSelector :: Selector
setOperationGroupSelector = mkSelector "setOperationGroup:"

-- | @Selector@ for @prioritizedZoneIDs@
prioritizedZoneIDsSelector :: Selector
prioritizedZoneIDsSelector = mkSelector "prioritizedZoneIDs"

-- | @Selector@ for @setPrioritizedZoneIDs:@
setPrioritizedZoneIDsSelector :: Selector
setPrioritizedZoneIDsSelector = mkSelector "setPrioritizedZoneIDs:"

