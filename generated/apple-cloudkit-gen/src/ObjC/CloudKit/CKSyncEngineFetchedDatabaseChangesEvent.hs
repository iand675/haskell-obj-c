{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A batch of database changes was fetched from the server.
--
-- If there are a lot of new changes on the server, then you might receive many of these events in a row.
--
-- The ordering of fetched changes is not guaranteed, but changes will typically be fetched from oldest to newest.
--
-- Generated bindings for @CKSyncEngineFetchedDatabaseChangesEvent@.
module ObjC.CloudKit.CKSyncEngineFetchedDatabaseChangesEvent
  ( CKSyncEngineFetchedDatabaseChangesEvent
  , IsCKSyncEngineFetchedDatabaseChangesEvent(..)
  , modifications
  , deletions
  , deletionsSelector
  , modificationsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- modifications@
modifications :: IsCKSyncEngineFetchedDatabaseChangesEvent ckSyncEngineFetchedDatabaseChangesEvent => ckSyncEngineFetchedDatabaseChangesEvent -> IO (Id NSArray)
modifications ckSyncEngineFetchedDatabaseChangesEvent =
  sendMessage ckSyncEngineFetchedDatabaseChangesEvent modificationsSelector

-- | @- deletions@
deletions :: IsCKSyncEngineFetchedDatabaseChangesEvent ckSyncEngineFetchedDatabaseChangesEvent => ckSyncEngineFetchedDatabaseChangesEvent -> IO (Id NSArray)
deletions ckSyncEngineFetchedDatabaseChangesEvent =
  sendMessage ckSyncEngineFetchedDatabaseChangesEvent deletionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @modifications@
modificationsSelector :: Selector '[] (Id NSArray)
modificationsSelector = mkSelector "modifications"

-- | @Selector@ for @deletions@
deletionsSelector :: Selector '[] (Id NSArray)
deletionsSelector = mkSelector "deletions"

