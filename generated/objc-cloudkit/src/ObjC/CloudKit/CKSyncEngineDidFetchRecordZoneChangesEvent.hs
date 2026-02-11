{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The sync engine finished fetching record zone changes from the server for a specific zone.
--
-- This might be a good signal to perform any post-processing tasks on a per-zone basis if necessary.
--
-- You should receive one @CKSyncEngineDidFetchRecordZoneChangesEvent@ for each @CKSyncEngineWillFetchRecordZoneChangesEvent@.
--
-- Generated bindings for @CKSyncEngineDidFetchRecordZoneChangesEvent@.
module ObjC.CloudKit.CKSyncEngineDidFetchRecordZoneChangesEvent
  ( CKSyncEngineDidFetchRecordZoneChangesEvent
  , IsCKSyncEngineDidFetchRecordZoneChangesEvent(..)
  , zoneID
  , error_
  , zoneIDSelector
  , errorSelector


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

-- | @- zoneID@
zoneID :: IsCKSyncEngineDidFetchRecordZoneChangesEvent ckSyncEngineDidFetchRecordZoneChangesEvent => ckSyncEngineDidFetchRecordZoneChangesEvent -> IO (Id CKRecordZoneID)
zoneID ckSyncEngineDidFetchRecordZoneChangesEvent  =
  sendMsg ckSyncEngineDidFetchRecordZoneChangesEvent (mkSelector "zoneID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- error@
error_ :: IsCKSyncEngineDidFetchRecordZoneChangesEvent ckSyncEngineDidFetchRecordZoneChangesEvent => ckSyncEngineDidFetchRecordZoneChangesEvent -> IO (Id NSError)
error_ ckSyncEngineDidFetchRecordZoneChangesEvent  =
  sendMsg ckSyncEngineDidFetchRecordZoneChangesEvent (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector
zoneIDSelector = mkSelector "zoneID"

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

