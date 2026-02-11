{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The sync engine is about to fetch record zone changes from the server for a specific zone.
--
-- This might be a good signal to prepare your local data store for incoming changes if necessary.
--
-- Generated bindings for @CKSyncEngineWillFetchRecordZoneChangesEvent@.
module ObjC.CloudKit.CKSyncEngineWillFetchRecordZoneChangesEvent
  ( CKSyncEngineWillFetchRecordZoneChangesEvent
  , IsCKSyncEngineWillFetchRecordZoneChangesEvent(..)
  , zoneID
  , zoneIDSelector


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
zoneID :: IsCKSyncEngineWillFetchRecordZoneChangesEvent ckSyncEngineWillFetchRecordZoneChangesEvent => ckSyncEngineWillFetchRecordZoneChangesEvent -> IO (Id CKRecordZoneID)
zoneID ckSyncEngineWillFetchRecordZoneChangesEvent  =
  sendMsg ckSyncEngineWillFetchRecordZoneChangesEvent (mkSelector "zoneID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector
zoneIDSelector = mkSelector "zoneID"

