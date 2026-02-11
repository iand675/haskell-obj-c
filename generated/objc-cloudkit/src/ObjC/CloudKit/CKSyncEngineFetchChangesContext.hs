{-# LANGUAGE PatternSynonyms #-}
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
  , reasonSelector
  , optionsSelector

  -- * Enum types
  , CKSyncEngineSyncReason(CKSyncEngineSyncReason)
  , pattern CKSyncEngineSyncReasonScheduled
  , pattern CKSyncEngineSyncReasonManual

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
import ObjC.CloudKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKSyncEngineFetchChangesContext ckSyncEngineFetchChangesContext => ckSyncEngineFetchChangesContext -> IO (Id CKSyncEngineFetchChangesContext)
init_ ckSyncEngineFetchChangesContext  =
  sendMsg ckSyncEngineFetchChangesContext (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKSyncEngineFetchChangesContext)
new  =
  do
    cls' <- getRequiredClass "CKSyncEngineFetchChangesContext"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The reason why the sync engine is attempting to fetch changes.
--
-- ObjC selector: @- reason@
reason :: IsCKSyncEngineFetchChangesContext ckSyncEngineFetchChangesContext => ckSyncEngineFetchChangesContext -> IO CKSyncEngineSyncReason
reason ckSyncEngineFetchChangesContext  =
  fmap (coerce :: CLong -> CKSyncEngineSyncReason) $ sendMsg ckSyncEngineFetchChangesContext (mkSelector "reason") retCLong []

-- | The options being used for this attempt to fetch changes.
--
-- ObjC selector: @- options@
options :: IsCKSyncEngineFetchChangesContext ckSyncEngineFetchChangesContext => ckSyncEngineFetchChangesContext -> IO (Id CKSyncEngineFetchChangesOptions)
options ckSyncEngineFetchChangesContext  =
  sendMsg ckSyncEngineFetchChangesContext (mkSelector "options") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @reason@
reasonSelector :: Selector
reasonSelector = mkSelector "reason"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

