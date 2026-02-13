{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A set of options to use when sending changes to the server.
--
-- Generated bindings for @CKSyncEngineSendChangesOptions@.
module ObjC.CloudKit.CKSyncEngineSendChangesOptions
  ( CKSyncEngineSendChangesOptions
  , IsCKSyncEngineSendChangesOptions(..)
  , initWithScope
  , scope
  , setScope
  , operationGroup
  , setOperationGroup
  , initWithScopeSelector
  , operationGroupSelector
  , scopeSelector
  , setOperationGroupSelector
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
initWithScope :: (IsCKSyncEngineSendChangesOptions ckSyncEngineSendChangesOptions, IsCKSyncEngineSendChangesScope scope) => ckSyncEngineSendChangesOptions -> scope -> IO (Id CKSyncEngineSendChangesOptions)
initWithScope ckSyncEngineSendChangesOptions scope =
  sendOwnedMessage ckSyncEngineSendChangesOptions initWithScopeSelector (toCKSyncEngineSendChangesScope scope)

-- | The scope in which to send changes to the server.
--
-- ObjC selector: @- scope@
scope :: IsCKSyncEngineSendChangesOptions ckSyncEngineSendChangesOptions => ckSyncEngineSendChangesOptions -> IO (Id CKSyncEngineSendChangesScope)
scope ckSyncEngineSendChangesOptions =
  sendMessage ckSyncEngineSendChangesOptions scopeSelector

-- | The scope in which to send changes to the server.
--
-- ObjC selector: @- setScope:@
setScope :: (IsCKSyncEngineSendChangesOptions ckSyncEngineSendChangesOptions, IsCKSyncEngineSendChangesScope value) => ckSyncEngineSendChangesOptions -> value -> IO ()
setScope ckSyncEngineSendChangesOptions value =
  sendMessage ckSyncEngineSendChangesOptions setScopeSelector (toCKSyncEngineSendChangesScope value)

-- | The operation group to use for the underlying operations when sending changes.
--
-- You might set an operation group with a particular name in order to help you analyze telemetry in the CloudKit Console. If you don't provide an operation group, a default one will be created for you.
--
-- ObjC selector: @- operationGroup@
operationGroup :: IsCKSyncEngineSendChangesOptions ckSyncEngineSendChangesOptions => ckSyncEngineSendChangesOptions -> IO (Id CKOperationGroup)
operationGroup ckSyncEngineSendChangesOptions =
  sendMessage ckSyncEngineSendChangesOptions operationGroupSelector

-- | The operation group to use for the underlying operations when sending changes.
--
-- You might set an operation group with a particular name in order to help you analyze telemetry in the CloudKit Console. If you don't provide an operation group, a default one will be created for you.
--
-- ObjC selector: @- setOperationGroup:@
setOperationGroup :: (IsCKSyncEngineSendChangesOptions ckSyncEngineSendChangesOptions, IsCKOperationGroup value) => ckSyncEngineSendChangesOptions -> value -> IO ()
setOperationGroup ckSyncEngineSendChangesOptions value =
  sendMessage ckSyncEngineSendChangesOptions setOperationGroupSelector (toCKOperationGroup value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithScope:@
initWithScopeSelector :: Selector '[Id CKSyncEngineSendChangesScope] (Id CKSyncEngineSendChangesOptions)
initWithScopeSelector = mkSelector "initWithScope:"

-- | @Selector@ for @scope@
scopeSelector :: Selector '[] (Id CKSyncEngineSendChangesScope)
scopeSelector = mkSelector "scope"

-- | @Selector@ for @setScope:@
setScopeSelector :: Selector '[Id CKSyncEngineSendChangesScope] ()
setScopeSelector = mkSelector "setScope:"

-- | @Selector@ for @operationGroup@
operationGroupSelector :: Selector '[] (Id CKOperationGroup)
operationGroupSelector = mkSelector "operationGroup"

-- | @Selector@ for @setOperationGroup:@
setOperationGroupSelector :: Selector '[Id CKOperationGroup] ()
setOperationGroupSelector = mkSelector "setOperationGroup:"

