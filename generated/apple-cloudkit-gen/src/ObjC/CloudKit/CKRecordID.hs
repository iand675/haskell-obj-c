{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKRecordID@.
module ObjC.CloudKit.CKRecordID
  ( CKRecordID
  , IsCKRecordID(..)
  , init_
  , new
  , initWithRecordName
  , initWithRecordName_zoneID
  , recordName
  , zoneID
  , initSelector
  , initWithRecordNameSelector
  , initWithRecordName_zoneIDSelector
  , newSelector
  , recordNameSelector
  , zoneIDSelector


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
init_ :: IsCKRecordID ckRecordID => ckRecordID -> IO (Id CKRecordID)
init_ ckRecordID =
  sendOwnedMessage ckRecordID initSelector

-- | @+ new@
new :: IO (Id CKRecordID)
new  =
  do
    cls' <- getRequiredClass "CKRecordID"
    sendOwnedClassMessage cls' newSelector

-- | Creates a record ID in the default zone
--
-- ObjC selector: @- initWithRecordName:@
initWithRecordName :: (IsCKRecordID ckRecordID, IsNSString recordName) => ckRecordID -> recordName -> IO (Id CKRecordID)
initWithRecordName ckRecordID recordName =
  sendOwnedMessage ckRecordID initWithRecordNameSelector (toNSString recordName)

-- | @- initWithRecordName:zoneID:@
initWithRecordName_zoneID :: (IsCKRecordID ckRecordID, IsNSString recordName, IsCKRecordZoneID zoneID) => ckRecordID -> recordName -> zoneID -> IO (Id CKRecordID)
initWithRecordName_zoneID ckRecordID recordName zoneID =
  sendOwnedMessage ckRecordID initWithRecordName_zoneIDSelector (toNSString recordName) (toCKRecordZoneID zoneID)

-- | @- recordName@
recordName :: IsCKRecordID ckRecordID => ckRecordID -> IO (Id NSString)
recordName ckRecordID =
  sendMessage ckRecordID recordNameSelector

-- | @- zoneID@
zoneID :: IsCKRecordID ckRecordID => ckRecordID -> IO (Id CKRecordZoneID)
zoneID ckRecordID =
  sendMessage ckRecordID zoneIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKRecordID)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKRecordID)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithRecordName:@
initWithRecordNameSelector :: Selector '[Id NSString] (Id CKRecordID)
initWithRecordNameSelector = mkSelector "initWithRecordName:"

-- | @Selector@ for @initWithRecordName:zoneID:@
initWithRecordName_zoneIDSelector :: Selector '[Id NSString, Id CKRecordZoneID] (Id CKRecordID)
initWithRecordName_zoneIDSelector = mkSelector "initWithRecordName:zoneID:"

-- | @Selector@ for @recordName@
recordNameSelector :: Selector '[] (Id NSString)
recordNameSelector = mkSelector "recordName"

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector '[] (Id CKRecordZoneID)
zoneIDSelector = mkSelector "zoneID"

