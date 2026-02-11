{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKRecordZone@.
module ObjC.CloudKit.CKRecordZone
  ( CKRecordZone
  , IsCKRecordZone(..)
  , defaultRecordZone
  , init_
  , new
  , initWithZoneName
  , initWithZoneID
  , zoneID
  , capabilities
  , share
  , encryptionScope
  , setEncryptionScope
  , defaultRecordZoneSelector
  , initSelector
  , newSelector
  , initWithZoneNameSelector
  , initWithZoneIDSelector
  , zoneIDSelector
  , capabilitiesSelector
  , shareSelector
  , encryptionScopeSelector
  , setEncryptionScopeSelector

  -- * Enum types
  , CKRecordZoneCapabilities(CKRecordZoneCapabilities)
  , pattern CKRecordZoneCapabilityFetchChanges
  , pattern CKRecordZoneCapabilityAtomic
  , pattern CKRecordZoneCapabilitySharing
  , pattern CKRecordZoneCapabilityZoneWideSharing
  , CKRecordZoneEncryptionScope(CKRecordZoneEncryptionScope)
  , pattern CKRecordZoneEncryptionScopePerRecord
  , pattern CKRecordZoneEncryptionScopePerZone

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
import ObjC.CloudKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ defaultRecordZone@
defaultRecordZone :: IO (Id CKRecordZone)
defaultRecordZone  =
  do
    cls' <- getRequiredClass "CKRecordZone"
    sendClassMsg cls' (mkSelector "defaultRecordZone") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsCKRecordZone ckRecordZone => ckRecordZone -> IO (Id CKRecordZone)
init_ ckRecordZone  =
  sendMsg ckRecordZone (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CKRecordZone)
new  =
  do
    cls' <- getRequiredClass "CKRecordZone"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithZoneName:@
initWithZoneName :: (IsCKRecordZone ckRecordZone, IsNSString zoneName) => ckRecordZone -> zoneName -> IO (Id CKRecordZone)
initWithZoneName ckRecordZone  zoneName =
withObjCPtr zoneName $ \raw_zoneName ->
    sendMsg ckRecordZone (mkSelector "initWithZoneName:") (retPtr retVoid) [argPtr (castPtr raw_zoneName :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithZoneID:@
initWithZoneID :: (IsCKRecordZone ckRecordZone, IsCKRecordZoneID zoneID) => ckRecordZone -> zoneID -> IO (Id CKRecordZone)
initWithZoneID ckRecordZone  zoneID =
withObjCPtr zoneID $ \raw_zoneID ->
    sendMsg ckRecordZone (mkSelector "initWithZoneID:") (retPtr retVoid) [argPtr (castPtr raw_zoneID :: Ptr ())] >>= ownedObject . castPtr

-- | @- zoneID@
zoneID :: IsCKRecordZone ckRecordZone => ckRecordZone -> IO (Id CKRecordZoneID)
zoneID ckRecordZone  =
  sendMsg ckRecordZone (mkSelector "zoneID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Capabilities on locally-created record zones are not valid until the record zone is saved. Capabilities on record zones fetched from the server are valid.
--
-- ObjC selector: @- capabilities@
capabilities :: IsCKRecordZone ckRecordZone => ckRecordZone -> IO CKRecordZoneCapabilities
capabilities ckRecordZone  =
  fmap (coerce :: CULong -> CKRecordZoneCapabilities) $ sendMsg ckRecordZone (mkSelector "capabilities") retCULong []

-- | The share property on a record zone will only be set on zones fetched from the server and only if a corresponding zone-wide share record for the zone exists on the server.
--
-- You can create a zone-wide share for a zone using @-[CKShare initWithRecordZoneID:]@.
--
-- Zone-wide sharing is only supported in zones with the @CKRecordZoneCapabilityZoneWideSharing@ sharing capability. You cannot share a zone if it already contains shared records.
--
-- ObjC selector: @- share@
share :: IsCKRecordZone ckRecordZone => ckRecordZone -> IO (Id CKReference)
share ckRecordZone  =
  sendMsg ckRecordZone (mkSelector "share") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The encryption scope determines the granularity at which encryption keys are stored within the zone.
--
-- Zone encryption scope defaults to @CKRecordZoneEncryptionScopePerRecord@ and can only be modified before zone creation. Attempting to change the encryption scope of an existing zone is invalid and will result in an error.
--
-- Zones using @CKRecordZoneEncryptionScopePerZone@ can only use zone-wide sharing and are not compatible with older device OS versions. Refer to @CKRecordZoneEncryptionScope@ for more info.
--
-- ObjC selector: @- encryptionScope@
encryptionScope :: IsCKRecordZone ckRecordZone => ckRecordZone -> IO CKRecordZoneEncryptionScope
encryptionScope ckRecordZone  =
  fmap (coerce :: CLong -> CKRecordZoneEncryptionScope) $ sendMsg ckRecordZone (mkSelector "encryptionScope") retCLong []

-- | The encryption scope determines the granularity at which encryption keys are stored within the zone.
--
-- Zone encryption scope defaults to @CKRecordZoneEncryptionScopePerRecord@ and can only be modified before zone creation. Attempting to change the encryption scope of an existing zone is invalid and will result in an error.
--
-- Zones using @CKRecordZoneEncryptionScopePerZone@ can only use zone-wide sharing and are not compatible with older device OS versions. Refer to @CKRecordZoneEncryptionScope@ for more info.
--
-- ObjC selector: @- setEncryptionScope:@
setEncryptionScope :: IsCKRecordZone ckRecordZone => ckRecordZone -> CKRecordZoneEncryptionScope -> IO ()
setEncryptionScope ckRecordZone  value =
  sendMsg ckRecordZone (mkSelector "setEncryptionScope:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultRecordZone@
defaultRecordZoneSelector :: Selector
defaultRecordZoneSelector = mkSelector "defaultRecordZone"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithZoneName:@
initWithZoneNameSelector :: Selector
initWithZoneNameSelector = mkSelector "initWithZoneName:"

-- | @Selector@ for @initWithZoneID:@
initWithZoneIDSelector :: Selector
initWithZoneIDSelector = mkSelector "initWithZoneID:"

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector
zoneIDSelector = mkSelector "zoneID"

-- | @Selector@ for @capabilities@
capabilitiesSelector :: Selector
capabilitiesSelector = mkSelector "capabilities"

-- | @Selector@ for @share@
shareSelector :: Selector
shareSelector = mkSelector "share"

-- | @Selector@ for @encryptionScope@
encryptionScopeSelector :: Selector
encryptionScopeSelector = mkSelector "encryptionScope"

-- | @Selector@ for @setEncryptionScope:@
setEncryptionScopeSelector :: Selector
setEncryptionScopeSelector = mkSelector "setEncryptionScope:"

