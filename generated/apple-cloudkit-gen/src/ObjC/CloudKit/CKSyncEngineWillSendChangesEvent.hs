{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The sync engine is about to send changes to the server.
--
-- Generated bindings for @CKSyncEngineWillSendChangesEvent@.
module ObjC.CloudKit.CKSyncEngineWillSendChangesEvent
  ( CKSyncEngineWillSendChangesEvent
  , IsCKSyncEngineWillSendChangesEvent(..)
  , context
  , contextSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- context@
context :: IsCKSyncEngineWillSendChangesEvent ckSyncEngineWillSendChangesEvent => ckSyncEngineWillSendChangesEvent -> IO (Id CKSyncEngineSendChangesContext)
context ckSyncEngineWillSendChangesEvent =
  sendMessage ckSyncEngineWillSendChangesEvent contextSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @context@
contextSelector :: Selector '[] (Id CKSyncEngineSendChangesContext)
contextSelector = mkSelector "context"

