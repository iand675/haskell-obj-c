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
  , initSelector
  , newSelector
  , recordZoneSelector
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

-- | @- init@
init_ :: IsCKSyncEngineFailedZoneSave ckSyncEngineFailedZoneSave => ckSyncEngineFailedZoneSave -> IO (Id CKSyncEngineFailedZoneSave)
init_ ckSyncEngineFailedZoneSave  =
  sendMsg ckSyncEngineFailedZoneSave (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKSyncEngineFailedZoneSave)
new  =
  do
    cls' <- getRequiredClass "CKSyncEngineFailedZoneSave"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- recordZone@
recordZone :: IsCKSyncEngineFailedZoneSave ckSyncEngineFailedZoneSave => ckSyncEngineFailedZoneSave -> IO (Id CKRecordZone)
recordZone ckSyncEngineFailedZoneSave  =
  sendMsg ckSyncEngineFailedZoneSave (mkSelector "recordZone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- error@
error_ :: IsCKSyncEngineFailedZoneSave ckSyncEngineFailedZoneSave => ckSyncEngineFailedZoneSave -> IO (Id NSError)
error_ ckSyncEngineFailedZoneSave  =
  sendMsg ckSyncEngineFailedZoneSave (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @recordZone@
recordZoneSelector :: Selector
recordZoneSelector = mkSelector "recordZone"

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

