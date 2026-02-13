{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKRecordZoneID@.
module ObjC.CloudKit.CKRecordZoneID
  ( CKRecordZoneID
  , IsCKRecordZoneID(..)
  , init_
  , new
  , initWithZoneName_ownerName
  , zoneName
  , ownerName
  , initSelector
  , initWithZoneName_ownerNameSelector
  , newSelector
  , ownerNameSelector
  , zoneNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Zone names must be 255 characters or less. Most UTF-8 characters are valid.
--
-- ObjC selector: @- init@
init_ :: IsCKRecordZoneID ckRecordZoneID => ckRecordZoneID -> IO (Id CKRecordZoneID)
init_ ckRecordZoneID =
  sendOwnedMessage ckRecordZoneID initSelector

-- | @+ new@
new :: IO (Id CKRecordZoneID)
new  =
  do
    cls' <- getRequiredClass "CKRecordZoneID"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithZoneName:ownerName:@
initWithZoneName_ownerName :: (IsCKRecordZoneID ckRecordZoneID, IsNSString zoneName, IsNSString ownerName) => ckRecordZoneID -> zoneName -> ownerName -> IO (Id CKRecordZoneID)
initWithZoneName_ownerName ckRecordZoneID zoneName ownerName =
  sendOwnedMessage ckRecordZoneID initWithZoneName_ownerNameSelector (toNSString zoneName) (toNSString ownerName)

-- | @- zoneName@
zoneName :: IsCKRecordZoneID ckRecordZoneID => ckRecordZoneID -> IO (Id NSString)
zoneName ckRecordZoneID =
  sendMessage ckRecordZoneID zoneNameSelector

-- | @- ownerName@
ownerName :: IsCKRecordZoneID ckRecordZoneID => ckRecordZoneID -> IO (Id NSString)
ownerName ckRecordZoneID =
  sendMessage ckRecordZoneID ownerNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKRecordZoneID)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKRecordZoneID)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithZoneName:ownerName:@
initWithZoneName_ownerNameSelector :: Selector '[Id NSString, Id NSString] (Id CKRecordZoneID)
initWithZoneName_ownerNameSelector = mkSelector "initWithZoneName:ownerName:"

-- | @Selector@ for @zoneName@
zoneNameSelector :: Selector '[] (Id NSString)
zoneNameSelector = mkSelector "zoneName"

-- | @Selector@ for @ownerName@
ownerNameSelector :: Selector '[] (Id NSString)
ownerNameSelector = mkSelector "ownerName"

