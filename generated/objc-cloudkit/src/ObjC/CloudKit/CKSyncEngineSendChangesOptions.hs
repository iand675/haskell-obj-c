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
  , scopeSelector
  , setScopeSelector
  , operationGroupSelector
  , setOperationGroupSelector


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
initWithScope :: (IsCKSyncEngineSendChangesOptions ckSyncEngineSendChangesOptions, IsCKSyncEngineSendChangesScope scope) => ckSyncEngineSendChangesOptions -> scope -> IO (Id CKSyncEngineSendChangesOptions)
initWithScope ckSyncEngineSendChangesOptions  scope =
withObjCPtr scope $ \raw_scope ->
    sendMsg ckSyncEngineSendChangesOptions (mkSelector "initWithScope:") (retPtr retVoid) [argPtr (castPtr raw_scope :: Ptr ())] >>= ownedObject . castPtr

-- | The scope in which to send changes to the server.
--
-- ObjC selector: @- scope@
scope :: IsCKSyncEngineSendChangesOptions ckSyncEngineSendChangesOptions => ckSyncEngineSendChangesOptions -> IO (Id CKSyncEngineSendChangesScope)
scope ckSyncEngineSendChangesOptions  =
  sendMsg ckSyncEngineSendChangesOptions (mkSelector "scope") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The scope in which to send changes to the server.
--
-- ObjC selector: @- setScope:@
setScope :: (IsCKSyncEngineSendChangesOptions ckSyncEngineSendChangesOptions, IsCKSyncEngineSendChangesScope value) => ckSyncEngineSendChangesOptions -> value -> IO ()
setScope ckSyncEngineSendChangesOptions  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckSyncEngineSendChangesOptions (mkSelector "setScope:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The operation group to use for the underlying operations when sending changes.
--
-- You might set an operation group with a particular name in order to help you analyze telemetry in the CloudKit Console. If you don't provide an operation group, a default one will be created for you.
--
-- ObjC selector: @- operationGroup@
operationGroup :: IsCKSyncEngineSendChangesOptions ckSyncEngineSendChangesOptions => ckSyncEngineSendChangesOptions -> IO (Id CKOperationGroup)
operationGroup ckSyncEngineSendChangesOptions  =
  sendMsg ckSyncEngineSendChangesOptions (mkSelector "operationGroup") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The operation group to use for the underlying operations when sending changes.
--
-- You might set an operation group with a particular name in order to help you analyze telemetry in the CloudKit Console. If you don't provide an operation group, a default one will be created for you.
--
-- ObjC selector: @- setOperationGroup:@
setOperationGroup :: (IsCKSyncEngineSendChangesOptions ckSyncEngineSendChangesOptions, IsCKOperationGroup value) => ckSyncEngineSendChangesOptions -> value -> IO ()
setOperationGroup ckSyncEngineSendChangesOptions  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckSyncEngineSendChangesOptions (mkSelector "setOperationGroup:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

