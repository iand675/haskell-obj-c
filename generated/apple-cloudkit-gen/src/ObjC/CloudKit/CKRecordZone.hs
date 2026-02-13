{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , capabilitiesSelector
  , defaultRecordZoneSelector
  , encryptionScopeSelector
  , initSelector
  , initWithZoneIDSelector
  , initWithZoneNameSelector
  , newSelector
  , setEncryptionScopeSelector
  , shareSelector
  , zoneIDSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' defaultRecordZoneSelector

-- | @- init@
init_ :: IsCKRecordZone ckRecordZone => ckRecordZone -> IO (Id CKRecordZone)
init_ ckRecordZone =
  sendOwnedMessage ckRecordZone initSelector

-- | @+ new@
new :: IO (Id CKRecordZone)
new  =
  do
    cls' <- getRequiredClass "CKRecordZone"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithZoneName:@
initWithZoneName :: (IsCKRecordZone ckRecordZone, IsNSString zoneName) => ckRecordZone -> zoneName -> IO (Id CKRecordZone)
initWithZoneName ckRecordZone zoneName =
  sendOwnedMessage ckRecordZone initWithZoneNameSelector (toNSString zoneName)

-- | @- initWithZoneID:@
initWithZoneID :: (IsCKRecordZone ckRecordZone, IsCKRecordZoneID zoneID) => ckRecordZone -> zoneID -> IO (Id CKRecordZone)
initWithZoneID ckRecordZone zoneID =
  sendOwnedMessage ckRecordZone initWithZoneIDSelector (toCKRecordZoneID zoneID)

-- | @- zoneID@
zoneID :: IsCKRecordZone ckRecordZone => ckRecordZone -> IO (Id CKRecordZoneID)
zoneID ckRecordZone =
  sendMessage ckRecordZone zoneIDSelector

-- | Capabilities on locally-created record zones are not valid until the record zone is saved. Capabilities on record zones fetched from the server are valid.
--
-- ObjC selector: @- capabilities@
capabilities :: IsCKRecordZone ckRecordZone => ckRecordZone -> IO CKRecordZoneCapabilities
capabilities ckRecordZone =
  sendMessage ckRecordZone capabilitiesSelector

-- | The share property on a record zone will only be set on zones fetched from the server and only if a corresponding zone-wide share record for the zone exists on the server.
--
-- You can create a zone-wide share for a zone using @-[CKShare initWithRecordZoneID:]@.
--
-- Zone-wide sharing is only supported in zones with the @CKRecordZoneCapabilityZoneWideSharing@ sharing capability. You cannot share a zone if it already contains shared records.
--
-- ObjC selector: @- share@
share :: IsCKRecordZone ckRecordZone => ckRecordZone -> IO (Id CKReference)
share ckRecordZone =
  sendMessage ckRecordZone shareSelector

-- | The encryption scope determines the granularity at which encryption keys are stored within the zone.
--
-- Zone encryption scope defaults to @CKRecordZoneEncryptionScopePerRecord@ and can only be modified before zone creation. Attempting to change the encryption scope of an existing zone is invalid and will result in an error.
--
-- Zones using @CKRecordZoneEncryptionScopePerZone@ can only use zone-wide sharing and are not compatible with older device OS versions. Refer to @CKRecordZoneEncryptionScope@ for more info.
--
-- ObjC selector: @- encryptionScope@
encryptionScope :: IsCKRecordZone ckRecordZone => ckRecordZone -> IO CKRecordZoneEncryptionScope
encryptionScope ckRecordZone =
  sendMessage ckRecordZone encryptionScopeSelector

-- | The encryption scope determines the granularity at which encryption keys are stored within the zone.
--
-- Zone encryption scope defaults to @CKRecordZoneEncryptionScopePerRecord@ and can only be modified before zone creation. Attempting to change the encryption scope of an existing zone is invalid and will result in an error.
--
-- Zones using @CKRecordZoneEncryptionScopePerZone@ can only use zone-wide sharing and are not compatible with older device OS versions. Refer to @CKRecordZoneEncryptionScope@ for more info.
--
-- ObjC selector: @- setEncryptionScope:@
setEncryptionScope :: IsCKRecordZone ckRecordZone => ckRecordZone -> CKRecordZoneEncryptionScope -> IO ()
setEncryptionScope ckRecordZone value =
  sendMessage ckRecordZone setEncryptionScopeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultRecordZone@
defaultRecordZoneSelector :: Selector '[] (Id CKRecordZone)
defaultRecordZoneSelector = mkSelector "defaultRecordZone"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKRecordZone)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CKRecordZone)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithZoneName:@
initWithZoneNameSelector :: Selector '[Id NSString] (Id CKRecordZone)
initWithZoneNameSelector = mkSelector "initWithZoneName:"

-- | @Selector@ for @initWithZoneID:@
initWithZoneIDSelector :: Selector '[Id CKRecordZoneID] (Id CKRecordZone)
initWithZoneIDSelector = mkSelector "initWithZoneID:"

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector '[] (Id CKRecordZoneID)
zoneIDSelector = mkSelector "zoneID"

-- | @Selector@ for @capabilities@
capabilitiesSelector :: Selector '[] CKRecordZoneCapabilities
capabilitiesSelector = mkSelector "capabilities"

-- | @Selector@ for @share@
shareSelector :: Selector '[] (Id CKReference)
shareSelector = mkSelector "share"

-- | @Selector@ for @encryptionScope@
encryptionScopeSelector :: Selector '[] CKRecordZoneEncryptionScope
encryptionScopeSelector = mkSelector "encryptionScope"

-- | @Selector@ for @setEncryptionScope:@
setEncryptionScopeSelector :: Selector '[CKRecordZoneEncryptionScope] ()
setEncryptionScopeSelector = mkSelector "setEncryptionScope:"

