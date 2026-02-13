{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKSyncEngineFailedZoneSave@.
module ObjC.CloudKit.CKSyncEngineFailedZoneSave
  ( CKSyncEngineFailedZoneSave
  , IsCKSyncEngineFailedZoneSave(..)
  , init_
  , new
  , recordZone
  , error_
  , errorSelector
  , initSelector
  , newSelector
  , recordZoneSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKSyncEngineFailedZoneSave ckSyncEngineFailedZoneSave => ckSyncEngineFailedZoneSave -> IO (Id CKSyncEngineFailedZoneSave)
init_ ckSyncEngineFailedZoneSave =
  sendOwnedMessage ckSyncEngineFailedZoneSave initSelector

-- | @+ new@
new :: IO (Id CKSyncEngineFailedZoneSave)
new  =
  do
    cls' <- getRequiredClass "CKSyncEngineFailedZoneSave"
    sendOwnedClassMessage cls' newSelector

-- | @- recordZone@
recordZone :: IsCKSyncEngineFailedZoneSave ckSyncEngineFailedZoneSave => ckSyncEngineFailedZoneSave -> IO (Id CKRecordZone)
recordZone ckSyncEngineFailedZoneSave =
  sendMessage ckSyncEngineFailedZoneSave recordZoneSelector

-- | @- error@
error_ :: IsCKSyncEngineFailedZoneSave ckSyncEngineFailedZoneSave => ckSyncEngineFailedZoneSave -> IO (Id NSError)
error_ ckSyncEngineFailedZoneSave =
  sendMessage ckSyncEngineFailedZoneSave errorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKSyncEngineFailedZoneSave)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKSyncEngineFailedZoneSave)
newSelector = mkSelector "new"

-- | @Selector@ for @recordZone@
recordZoneSelector :: Selector '[] (Id CKRecordZone)
recordZoneSelector = mkSelector "recordZone"

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

