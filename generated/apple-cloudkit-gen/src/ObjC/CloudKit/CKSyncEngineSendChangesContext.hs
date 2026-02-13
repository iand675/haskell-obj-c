{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The context of an attempt to send changes to the server.
--
-- The sync engine might attempt to send changes to the server for many reasons. For example, if you call ``CKSyncEngine/sendChanges(_:)``, it'll try to send changes immediately. Or if you add pending changes to the state, it'll schedule a sync and send changes when the scheduler task runs. This object represents one of those attempts to send changes.
--
-- Generated bindings for @CKSyncEngineSendChangesContext@.
module ObjC.CloudKit.CKSyncEngineSendChangesContext
  ( CKSyncEngineSendChangesContext
  , IsCKSyncEngineSendChangesContext(..)
  , init_
  , new
  , reason
  , options
  , initSelector
  , newSelector
  , optionsSelector
  , reasonSelector

  -- * Enum types
  , CKSyncEngineSyncReason(CKSyncEngineSyncReason)
  , pattern CKSyncEngineSyncReasonScheduled
  , pattern CKSyncEngineSyncReasonManual

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.CloudKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKSyncEngineSendChangesContext ckSyncEngineSendChangesContext => ckSyncEngineSendChangesContext -> IO (Id CKSyncEngineSendChangesContext)
init_ ckSyncEngineSendChangesContext =
  sendOwnedMessage ckSyncEngineSendChangesContext initSelector

-- | @+ new@
new :: IO (Id CKSyncEngineSendChangesContext)
new  =
  do
    cls' <- getRequiredClass "CKSyncEngineSendChangesContext"
    sendOwnedClassMessage cls' newSelector

-- | The reason why the sync engine is attempting to send changes.
--
-- ObjC selector: @- reason@
reason :: IsCKSyncEngineSendChangesContext ckSyncEngineSendChangesContext => ckSyncEngineSendChangesContext -> IO CKSyncEngineSyncReason
reason ckSyncEngineSendChangesContext =
  sendMessage ckSyncEngineSendChangesContext reasonSelector

-- | The options being used for this attempt to send changes.
--
-- ObjC selector: @- options@
options :: IsCKSyncEngineSendChangesContext ckSyncEngineSendChangesContext => ckSyncEngineSendChangesContext -> IO (Id CKSyncEngineSendChangesOptions)
options ckSyncEngineSendChangesContext =
  sendMessage ckSyncEngineSendChangesContext optionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKSyncEngineSendChangesContext)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKSyncEngineSendChangesContext)
newSelector = mkSelector "new"

-- | @Selector@ for @reason@
reasonSelector :: Selector '[] CKSyncEngineSyncReason
reasonSelector = mkSelector "reason"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] (Id CKSyncEngineSendChangesOptions)
optionsSelector = mkSelector "options"

