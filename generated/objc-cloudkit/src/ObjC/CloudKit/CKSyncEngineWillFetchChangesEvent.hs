{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The sync engine is about to fetch changes from the server.
--
-- This might be a good signal to prepare your local data store for incoming changes if necessary. The changes themselves will be delivered via @CKSyncEngineFetchedDatabaseChanges@ and @CKSyncEngineFetchedRecordZoneChangesEvent@.
--
-- Note that this event might not always occur every time you call @fetchChanges@. For example, if you call @fetchChanges@ concurrently while the engine is already fetching changes, this event might not be sent. Similarly, if there's no logged-in account, the engine might short-circuit the call to @fetchChanges@, and this event won't be sent.
--
-- Generated bindings for @CKSyncEngineWillFetchChangesEvent@.
module ObjC.CloudKit.CKSyncEngineWillFetchChangesEvent
  ( CKSyncEngineWillFetchChangesEvent
  , IsCKSyncEngineWillFetchChangesEvent(..)
  , context
  , contextSelector


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

-- | @- context@
context :: IsCKSyncEngineWillFetchChangesEvent ckSyncEngineWillFetchChangesEvent => ckSyncEngineWillFetchChangesEvent -> IO (Id CKSyncEngineFetchChangesContext)
context ckSyncEngineWillFetchChangesEvent  =
  sendMsg ckSyncEngineWillFetchChangesEvent (mkSelector "context") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @context@
contextSelector :: Selector
contextSelector = mkSelector "context"

