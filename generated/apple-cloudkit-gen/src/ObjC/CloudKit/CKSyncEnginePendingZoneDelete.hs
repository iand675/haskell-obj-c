{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A zone delete that needs to be sent to the server.
--
-- Generated bindings for @CKSyncEnginePendingZoneDelete@.
module ObjC.CloudKit.CKSyncEnginePendingZoneDelete
  ( CKSyncEnginePendingZoneDelete
  , IsCKSyncEnginePendingZoneDelete(..)
  , initWithZoneID
  , initWithZoneIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithZoneID:@
initWithZoneID :: (IsCKSyncEnginePendingZoneDelete ckSyncEnginePendingZoneDelete, IsCKRecordZoneID zoneID) => ckSyncEnginePendingZoneDelete -> zoneID -> IO (Id CKSyncEnginePendingZoneDelete)
initWithZoneID ckSyncEnginePendingZoneDelete zoneID =
  sendOwnedMessage ckSyncEnginePendingZoneDelete initWithZoneIDSelector (toCKRecordZoneID zoneID)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithZoneID:@
initWithZoneIDSelector :: Selector '[Id CKRecordZoneID] (Id CKSyncEnginePendingZoneDelete)
initWithZoneIDSelector = mkSelector "initWithZoneID:"

