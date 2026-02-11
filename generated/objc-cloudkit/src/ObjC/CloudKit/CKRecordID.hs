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
  , newSelector
  , initWithRecordNameSelector
  , initWithRecordName_zoneIDSelector
  , recordNameSelector
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

-- | @- init@
init_ :: IsCKRecordID ckRecordID => ckRecordID -> IO (Id CKRecordID)
init_ ckRecordID  =
  sendMsg ckRecordID (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKRecordID)
new  =
  do
    cls' <- getRequiredClass "CKRecordID"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates a record ID in the default zone
--
-- ObjC selector: @- initWithRecordName:@
initWithRecordName :: (IsCKRecordID ckRecordID, IsNSString recordName) => ckRecordID -> recordName -> IO (Id CKRecordID)
initWithRecordName ckRecordID  recordName =
withObjCPtr recordName $ \raw_recordName ->
    sendMsg ckRecordID (mkSelector "initWithRecordName:") (retPtr retVoid) [argPtr (castPtr raw_recordName :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithRecordName:zoneID:@
initWithRecordName_zoneID :: (IsCKRecordID ckRecordID, IsNSString recordName, IsCKRecordZoneID zoneID) => ckRecordID -> recordName -> zoneID -> IO (Id CKRecordID)
initWithRecordName_zoneID ckRecordID  recordName zoneID =
withObjCPtr recordName $ \raw_recordName ->
  withObjCPtr zoneID $ \raw_zoneID ->
      sendMsg ckRecordID (mkSelector "initWithRecordName:zoneID:") (retPtr retVoid) [argPtr (castPtr raw_recordName :: Ptr ()), argPtr (castPtr raw_zoneID :: Ptr ())] >>= ownedObject . castPtr

-- | @- recordName@
recordName :: IsCKRecordID ckRecordID => ckRecordID -> IO (Id NSString)
recordName ckRecordID  =
  sendMsg ckRecordID (mkSelector "recordName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- zoneID@
zoneID :: IsCKRecordID ckRecordID => ckRecordID -> IO (Id CKRecordZoneID)
zoneID ckRecordID  =
  sendMsg ckRecordID (mkSelector "zoneID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithRecordName:@
initWithRecordNameSelector :: Selector
initWithRecordNameSelector = mkSelector "initWithRecordName:"

-- | @Selector@ for @initWithRecordName:zoneID:@
initWithRecordName_zoneIDSelector :: Selector
initWithRecordName_zoneIDSelector = mkSelector "initWithRecordName:zoneID:"

-- | @Selector@ for @recordName@
recordNameSelector :: Selector
recordNameSelector = mkSelector "recordName"

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector
zoneIDSelector = mkSelector "zoneID"

