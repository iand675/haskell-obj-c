{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The context of an attempt to fetch changes from the server.
--
-- The sync engine might attempt to fetch changes to the server for many reasons. For example, if you call ``CKSyncEngine/fetchChanges(_:)``, it'll try to fetch changes immediately. Or if it receives a push notification, it'll schedule a sync and fetch changes when the scheduler task runs. This object represents one of those attempts to fetch changes.
--
-- Generated bindings for @CKSyncEngineFetchChangesContext@.
module ObjC.CloudKit.CKSyncEngineFetchChangesContext
  ( CKSyncEngineFetchChangesContext
  , IsCKSyncEngineFetchChangesContext(..)
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
init_ :: IsCKSyncEngineFetchChangesContext ckSyncEngineFetchChangesContext => ckSyncEngineFetchChangesContext -> IO (Id CKSyncEngineFetchChangesContext)
init_ ckSyncEngineFetchChangesContext =
  sendOwnedMessage ckSyncEngineFetchChangesContext initSelector

-- | @+ new@
new :: IO (Id CKSyncEngineFetchChangesContext)
new  =
  do
    cls' <- getRequiredClass "CKSyncEngineFetchChangesContext"
    sendOwnedClassMessage cls' newSelector

-- | The reason why the sync engine is attempting to fetch changes.
--
-- ObjC selector: @- reason@
reason :: IsCKSyncEngineFetchChangesContext ckSyncEngineFetchChangesContext => ckSyncEngineFetchChangesContext -> IO CKSyncEngineSyncReason
reason ckSyncEngineFetchChangesContext =
  sendMessage ckSyncEngineFetchChangesContext reasonSelector

-- | The options being used for this attempt to fetch changes.
--
-- ObjC selector: @- options@
options :: IsCKSyncEngineFetchChangesContext ckSyncEngineFetchChangesContext => ckSyncEngineFetchChangesContext -> IO (Id CKSyncEngineFetchChangesOptions)
options ckSyncEngineFetchChangesContext =
  sendMessage ckSyncEngineFetchChangesContext optionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKSyncEngineFetchChangesContext)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKSyncEngineFetchChangesContext)
newSelector = mkSelector "new"

-- | @Selector@ for @reason@
reasonSelector :: Selector '[] CKSyncEngineSyncReason
reasonSelector = mkSelector "reason"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] (Id CKSyncEngineFetchChangesOptions)
optionsSelector = mkSelector "options"

