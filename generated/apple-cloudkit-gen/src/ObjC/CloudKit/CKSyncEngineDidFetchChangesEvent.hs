{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The sync engine finished fetching changes from the server.
--
-- This might be a good signal to perform any post-processing tasks required after persisting fetched changes to disk.
--
-- You should receive one @CKSyncEngineDidFetchChangesEvent@ for each @CKSyncEngineWillFetchChangesEvent@.
--
-- Generated bindings for @CKSyncEngineDidFetchChangesEvent@.
module ObjC.CloudKit.CKSyncEngineDidFetchChangesEvent
  ( CKSyncEngineDidFetchChangesEvent
  , IsCKSyncEngineDidFetchChangesEvent(..)
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
context :: IsCKSyncEngineDidFetchChangesEvent ckSyncEngineDidFetchChangesEvent => ckSyncEngineDidFetchChangesEvent -> IO (Id CKSyncEngineFetchChangesContext)
context ckSyncEngineDidFetchChangesEvent =
  sendMessage ckSyncEngineDidFetchChangesEvent contextSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @context@
contextSelector :: Selector '[] (Id CKSyncEngineFetchChangesContext)
contextSelector = mkSelector "context"

