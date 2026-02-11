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
  , modificationsSelector
  , deletionsSelector


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

-- | @- modifications@
modifications :: IsCKSyncEngineFetchedRecordZoneChangesEvent ckSyncEngineFetchedRecordZoneChangesEvent => ckSyncEngineFetchedRecordZoneChangesEvent -> IO (Id NSArray)
modifications ckSyncEngineFetchedRecordZoneChangesEvent  =
  sendMsg ckSyncEngineFetchedRecordZoneChangesEvent (mkSelector "modifications") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- deletions@
deletions :: IsCKSyncEngineFetchedRecordZoneChangesEvent ckSyncEngineFetchedRecordZoneChangesEvent => ckSyncEngineFetchedRecordZoneChangesEvent -> IO (Id NSArray)
deletions ckSyncEngineFetchedRecordZoneChangesEvent  =
  sendMsg ckSyncEngineFetchedRecordZoneChangesEvent (mkSelector "deletions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @modifications@
modificationsSelector :: Selector
modificationsSelector = mkSelector "modifications"

-- | @Selector@ for @deletions@
deletionsSelector :: Selector
deletionsSelector = mkSelector "deletions"

