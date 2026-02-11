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
  , newSelector
  , initWithZoneName_ownerNameSelector
  , zoneNameSelector
  , ownerNameSelector


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

-- | Zone names must be 255 characters or less. Most UTF-8 characters are valid.
--
-- ObjC selector: @- init@
init_ :: IsCKRecordZoneID ckRecordZoneID => ckRecordZoneID -> IO (Id CKRecordZoneID)
init_ ckRecordZoneID  =
  sendMsg ckRecordZoneID (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKRecordZoneID)
new  =
  do
    cls' <- getRequiredClass "CKRecordZoneID"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithZoneName:ownerName:@
initWithZoneName_ownerName :: (IsCKRecordZoneID ckRecordZoneID, IsNSString zoneName, IsNSString ownerName) => ckRecordZoneID -> zoneName -> ownerName -> IO (Id CKRecordZoneID)
initWithZoneName_ownerName ckRecordZoneID  zoneName ownerName =
withObjCPtr zoneName $ \raw_zoneName ->
  withObjCPtr ownerName $ \raw_ownerName ->
      sendMsg ckRecordZoneID (mkSelector "initWithZoneName:ownerName:") (retPtr retVoid) [argPtr (castPtr raw_zoneName :: Ptr ()), argPtr (castPtr raw_ownerName :: Ptr ())] >>= ownedObject . castPtr

-- | @- zoneName@
zoneName :: IsCKRecordZoneID ckRecordZoneID => ckRecordZoneID -> IO (Id NSString)
zoneName ckRecordZoneID  =
  sendMsg ckRecordZoneID (mkSelector "zoneName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- ownerName@
ownerName :: IsCKRecordZoneID ckRecordZoneID => ckRecordZoneID -> IO (Id NSString)
ownerName ckRecordZoneID  =
  sendMsg ckRecordZoneID (mkSelector "ownerName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithZoneName:ownerName:@
initWithZoneName_ownerNameSelector :: Selector
initWithZoneName_ownerNameSelector = mkSelector "initWithZoneName:ownerName:"

-- | @Selector@ for @zoneName@
zoneNameSelector :: Selector
zoneNameSelector = mkSelector "zoneName"

-- | @Selector@ for @ownerName@
ownerNameSelector :: Selector
ownerNameSelector = mkSelector "ownerName"

