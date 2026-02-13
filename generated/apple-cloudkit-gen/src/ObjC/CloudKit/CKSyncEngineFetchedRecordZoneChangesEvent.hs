{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A batch of record zone changes was fetched from the server.
--
-- If there are a lot of new changes on the server, then you might receive many of these events in a row.
--
-- The ordering of fetched changes is not guaranteed, but changes will typically be fetched from oldest to newest.
--
-- Generated bindings for @CKSyncEngineFetchedRecordZoneChangesEvent@.
module ObjC.CloudKit.CKSyncEngineFetchedRecordZoneChangesEvent
  ( CKSyncEngineFetchedRecordZoneChangesEvent
  , IsCKSyncEngineFetchedRecordZoneChangesEvent(..)
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
modifications :: IsCKSyncEngineFetchedRecordZoneChangesEvent ckSyncEngineFetchedRecordZoneChangesEvent => ckSyncEngineFetchedRecordZoneChangesEvent -> IO (Id NSArray)
modifications ckSyncEngineFetchedRecordZoneChangesEvent =
  sendMessage ckSyncEngineFetchedRecordZoneChangesEvent modificationsSelector

-- | @- deletions@
deletions :: IsCKSyncEngineFetchedRecordZoneChangesEvent ckSyncEngineFetchedRecordZoneChangesEvent => ckSyncEngineFetchedRecordZoneChangesEvent -> IO (Id NSArray)
deletions ckSyncEngineFetchedRecordZoneChangesEvent =
  sendMessage ckSyncEngineFetchedRecordZoneChangesEvent deletionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @modifications@
modificationsSelector :: Selector '[] (Id NSArray)
modificationsSelector = mkSelector "modifications"

-- | @Selector@ for @deletions@
deletionsSelector :: Selector '[] (Id NSArray)
deletionsSelector = mkSelector "deletions"

