{-# LANGUAGE DataKinds #-}
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
  , operationGroupSelector
  , prioritizedZoneIDsSelector
  , scopeSelector
  , setOperationGroupSelector
  , setPrioritizedZoneIDsSelector
  , setScopeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a set of options with the specific scope. If no scope is provided, the default scope will include everything.
--
-- ObjC selector: @- initWithScope:@
initWithScope :: (IsCKSyncEngineFetchChangesOptions ckSyncEngineFetchChangesOptions, IsCKSyncEngineFetchChangesScope scope) => ckSyncEngineFetchChangesOptions -> scope -> IO (Id CKSyncEngineFetchChangesOptions)
initWithScope ckSyncEngineFetchChangesOptions scope =
  sendOwnedMessage ckSyncEngineFetchChangesOptions initWithScopeSelector (toCKSyncEngineFetchChangesScope scope)

-- | The scope in which to fetch changes from the server.
--
-- ObjC selector: @- scope@
scope :: IsCKSyncEngineFetchChangesOptions ckSyncEngineFetchChangesOptions => ckSyncEngineFetchChangesOptions -> IO (Id CKSyncEngineFetchChangesScope)
scope ckSyncEngineFetchChangesOptions =
  sendMessage ckSyncEngineFetchChangesOptions scopeSelector

-- | The scope in which to fetch changes from the server.
--
-- ObjC selector: @- setScope:@
setScope :: (IsCKSyncEngineFetchChangesOptions ckSyncEngineFetchChangesOptions, IsCKSyncEngineFetchChangesScope value) => ckSyncEngineFetchChangesOptions -> value -> IO ()
setScope ckSyncEngineFetchChangesOptions value =
  sendMessage ckSyncEngineFetchChangesOptions setScopeSelector (toCKSyncEngineFetchChangesScope value)

-- | The operation group to use for the underlying operations when fetching changes.
--
-- You might set an operation group with a particular name in order to help you analyze telemetry in the CloudKit Console. If you don't provide an operation group, a default one will be created for you.
--
-- ObjC selector: @- operationGroup@
operationGroup :: IsCKSyncEngineFetchChangesOptions ckSyncEngineFetchChangesOptions => ckSyncEngineFetchChangesOptions -> IO (Id CKOperationGroup)
operationGroup ckSyncEngineFetchChangesOptions =
  sendMessage ckSyncEngineFetchChangesOptions operationGroupSelector

-- | The operation group to use for the underlying operations when fetching changes.
--
-- You might set an operation group with a particular name in order to help you analyze telemetry in the CloudKit Console. If you don't provide an operation group, a default one will be created for you.
--
-- ObjC selector: @- setOperationGroup:@
setOperationGroup :: (IsCKSyncEngineFetchChangesOptions ckSyncEngineFetchChangesOptions, IsCKOperationGroup value) => ckSyncEngineFetchChangesOptions -> value -> IO ()
setOperationGroup ckSyncEngineFetchChangesOptions value =
  sendMessage ckSyncEngineFetchChangesOptions setOperationGroupSelector (toCKOperationGroup value)

-- | A list of zones that should be prioritized over others while fetching changes.
--
-- @CKSyncEngine@ will fetch changes for the zones in this list first before any other zones. You might use this to prioritize a specific set of zones for initial sync. You could also prioritize the object currently showing in the UI by putting it first in this list.
--
-- Any zones not included in this list will be prioritized in a default manner. If a zone in this list has no changes to fetch, then that zone will be ignored.
--
-- ObjC selector: @- prioritizedZoneIDs@
prioritizedZoneIDs :: IsCKSyncEngineFetchChangesOptions ckSyncEngineFetchChangesOptions => ckSyncEngineFetchChangesOptions -> IO (Id NSArray)
prioritizedZoneIDs ckSyncEngineFetchChangesOptions =
  sendMessage ckSyncEngineFetchChangesOptions prioritizedZoneIDsSelector

-- | A list of zones that should be prioritized over others while fetching changes.
--
-- @CKSyncEngine@ will fetch changes for the zones in this list first before any other zones. You might use this to prioritize a specific set of zones for initial sync. You could also prioritize the object currently showing in the UI by putting it first in this list.
--
-- Any zones not included in this list will be prioritized in a default manner. If a zone in this list has no changes to fetch, then that zone will be ignored.
--
-- ObjC selector: @- setPrioritizedZoneIDs:@
setPrioritizedZoneIDs :: (IsCKSyncEngineFetchChangesOptions ckSyncEngineFetchChangesOptions, IsNSArray value) => ckSyncEngineFetchChangesOptions -> value -> IO ()
setPrioritizedZoneIDs ckSyncEngineFetchChangesOptions value =
  sendMessage ckSyncEngineFetchChangesOptions setPrioritizedZoneIDsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithScope:@
initWithScopeSelector :: Selector '[Id CKSyncEngineFetchChangesScope] (Id CKSyncEngineFetchChangesOptions)
initWithScopeSelector = mkSelector "initWithScope:"

-- | @Selector@ for @scope@
scopeSelector :: Selector '[] (Id CKSyncEngineFetchChangesScope)
scopeSelector = mkSelector "scope"

-- | @Selector@ for @setScope:@
setScopeSelector :: Selector '[Id CKSyncEngineFetchChangesScope] ()
setScopeSelector = mkSelector "setScope:"

-- | @Selector@ for @operationGroup@
operationGroupSelector :: Selector '[] (Id CKOperationGroup)
operationGroupSelector = mkSelector "operationGroup"

-- | @Selector@ for @setOperationGroup:@
setOperationGroupSelector :: Selector '[Id CKOperationGroup] ()
setOperationGroupSelector = mkSelector "setOperationGroup:"

-- | @Selector@ for @prioritizedZoneIDs@
prioritizedZoneIDsSelector :: Selector '[] (Id NSArray)
prioritizedZoneIDsSelector = mkSelector "prioritizedZoneIDs"

-- | @Selector@ for @setPrioritizedZoneIDs:@
setPrioritizedZoneIDsSelector :: Selector '[Id NSArray] ()
setPrioritizedZoneIDsSelector = mkSelector "setPrioritizedZoneIDs:"

