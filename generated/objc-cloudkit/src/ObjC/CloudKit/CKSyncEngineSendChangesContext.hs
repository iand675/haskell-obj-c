{-# LANGUAGE PatternSynonyms #-}
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
init_ :: IsCKSyncEngineSendChangesContext ckSyncEngineSendChangesContext => ckSyncEngineSendChangesContext -> IO (Id CKSyncEngineSendChangesContext)
init_ ckSyncEngineSendChangesContext  =
  sendMsg ckSyncEngineSendChangesContext (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKSyncEngineSendChangesContext)
new  =
  do
    cls' <- getRequiredClass "CKSyncEngineSendChangesContext"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The reason why the sync engine is attempting to send changes.
--
-- ObjC selector: @- reason@
reason :: IsCKSyncEngineSendChangesContext ckSyncEngineSendChangesContext => ckSyncEngineSendChangesContext -> IO CKSyncEngineSyncReason
reason ckSyncEngineSendChangesContext  =
  fmap (coerce :: CLong -> CKSyncEngineSyncReason) $ sendMsg ckSyncEngineSendChangesContext (mkSelector "reason") retCLong []

-- | The options being used for this attempt to send changes.
--
-- ObjC selector: @- options@
options :: IsCKSyncEngineSendChangesContext ckSyncEngineSendChangesContext => ckSyncEngineSendChangesContext -> IO (Id CKSyncEngineSendChangesOptions)
options ckSyncEngineSendChangesContext  =
  sendMsg ckSyncEngineSendChangesContext (mkSelector "options") (retPtr retVoid) [] >>= retainedObject . castPtr

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

