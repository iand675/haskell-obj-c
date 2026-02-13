{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The sync engine finished sending changes to the server.
--
-- You should receive one @CKSyncEngineDidSendChangesEvent@ for every @CKSyncEngineWillSendChangesEvent@.
--
-- Generated bindings for @CKSyncEngineDidSendChangesEvent@.
module ObjC.CloudKit.CKSyncEngineDidSendChangesEvent
  ( CKSyncEngineDidSendChangesEvent
  , IsCKSyncEngineDidSendChangesEvent(..)
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
context :: IsCKSyncEngineDidSendChangesEvent ckSyncEngineDidSendChangesEvent => ckSyncEngineDidSendChangesEvent -> IO (Id CKSyncEngineSendChangesContext)
context ckSyncEngineDidSendChangesEvent =
  sendMessage ckSyncEngineDidSendChangesEvent contextSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @context@
contextSelector :: Selector '[] (Id CKSyncEngineSendChangesContext)
contextSelector = mkSelector "context"

