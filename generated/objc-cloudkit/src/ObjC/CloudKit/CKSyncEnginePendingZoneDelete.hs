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

-- | @- initWithZoneID:@
initWithZoneID :: (IsCKSyncEnginePendingZoneDelete ckSyncEnginePendingZoneDelete, IsCKRecordZoneID zoneID) => ckSyncEnginePendingZoneDelete -> zoneID -> IO (Id CKSyncEnginePendingZoneDelete)
initWithZoneID ckSyncEnginePendingZoneDelete  zoneID =
withObjCPtr zoneID $ \raw_zoneID ->
    sendMsg ckSyncEnginePendingZoneDelete (mkSelector "initWithZoneID:") (retPtr retVoid) [argPtr (castPtr raw_zoneID :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithZoneID:@
initWithZoneIDSelector :: Selector
initWithZoneIDSelector = mkSelector "initWithZoneID:"

