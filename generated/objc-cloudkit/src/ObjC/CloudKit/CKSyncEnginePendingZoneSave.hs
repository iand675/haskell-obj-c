{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A zone save that needs to be sent to the server.
--
-- Generated bindings for @CKSyncEnginePendingZoneSave@.
module ObjC.CloudKit.CKSyncEnginePendingZoneSave
  ( CKSyncEnginePendingZoneSave
  , IsCKSyncEnginePendingZoneSave(..)
  , initWithZone
  , zone
  , initWithZoneSelector
  , zoneSelector


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

-- | @- initWithZone:@
initWithZone :: (IsCKSyncEnginePendingZoneSave ckSyncEnginePendingZoneSave, IsCKRecordZone zone) => ckSyncEnginePendingZoneSave -> zone -> IO (Id CKSyncEnginePendingZoneSave)
initWithZone ckSyncEnginePendingZoneSave  zone =
withObjCPtr zone $ \raw_zone ->
    sendMsg ckSyncEnginePendingZoneSave (mkSelector "initWithZone:") (retPtr retVoid) [argPtr (castPtr raw_zone :: Ptr ())] >>= ownedObject . castPtr

-- | @- zone@
zone :: IsCKSyncEnginePendingZoneSave ckSyncEnginePendingZoneSave => ckSyncEnginePendingZoneSave -> IO (Id CKRecordZone)
zone ckSyncEnginePendingZoneSave  =
  sendMsg ckSyncEnginePendingZoneSave (mkSelector "zone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithZone:@
initWithZoneSelector :: Selector
initWithZoneSelector = mkSelector "initWithZone:"

-- | @Selector@ for @zone@
zoneSelector :: Selector
zoneSelector = mkSelector "zone"

