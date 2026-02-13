{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLBeaconRegion@.
module ObjC.CoreLocation.CLBeaconRegion
  ( CLBeaconRegion
  , IsCLBeaconRegion(..)
  , initWithUUID_identifier
  , initWithProximityUUID_identifier
  , initWithUUID_major_identifier
  , initWithProximityUUID_major_identifier
  , initWithUUID_major_minor_identifier
  , initWithProximityUUID_major_minor_identifier
  , initWithBeaconIdentityConstraint_identifier
  , peripheralDataWithMeasuredPower
  , beaconIdentityConstraint
  , uuid
  , proximityUUID
  , major
  , minor
  , notifyEntryStateOnDisplay
  , setNotifyEntryStateOnDisplay
  , beaconIdentityConstraintSelector
  , initWithBeaconIdentityConstraint_identifierSelector
  , initWithProximityUUID_identifierSelector
  , initWithProximityUUID_major_identifierSelector
  , initWithProximityUUID_major_minor_identifierSelector
  , initWithUUID_identifierSelector
  , initWithUUID_major_identifierSelector
  , initWithUUID_major_minor_identifierSelector
  , majorSelector
  , minorSelector
  , notifyEntryStateOnDisplaySelector
  , peripheralDataWithMeasuredPowerSelector
  , proximityUUIDSelector
  , setNotifyEntryStateOnDisplaySelector
  , uuidSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithUUID:identifier:@
initWithUUID_identifier :: (IsCLBeaconRegion clBeaconRegion, IsNSUUID uuid, IsNSString identifier) => clBeaconRegion -> uuid -> identifier -> IO (Id CLBeaconRegion)
initWithUUID_identifier clBeaconRegion uuid identifier =
  sendOwnedMessage clBeaconRegion initWithUUID_identifierSelector (toNSUUID uuid) (toNSString identifier)

-- | @- initWithProximityUUID:identifier:@
initWithProximityUUID_identifier :: (IsCLBeaconRegion clBeaconRegion, IsNSUUID proximityUUID, IsNSString identifier) => clBeaconRegion -> proximityUUID -> identifier -> IO (Id CLBeaconRegion)
initWithProximityUUID_identifier clBeaconRegion proximityUUID identifier =
  sendOwnedMessage clBeaconRegion initWithProximityUUID_identifierSelector (toNSUUID proximityUUID) (toNSString identifier)

-- | @- initWithUUID:major:identifier:@
initWithUUID_major_identifier :: (IsCLBeaconRegion clBeaconRegion, IsNSUUID uuid, IsNSString identifier) => clBeaconRegion -> uuid -> CUShort -> identifier -> IO (Id CLBeaconRegion)
initWithUUID_major_identifier clBeaconRegion uuid major identifier =
  sendOwnedMessage clBeaconRegion initWithUUID_major_identifierSelector (toNSUUID uuid) major (toNSString identifier)

-- | @- initWithProximityUUID:major:identifier:@
initWithProximityUUID_major_identifier :: (IsCLBeaconRegion clBeaconRegion, IsNSUUID proximityUUID, IsNSString identifier) => clBeaconRegion -> proximityUUID -> CUShort -> identifier -> IO (Id CLBeaconRegion)
initWithProximityUUID_major_identifier clBeaconRegion proximityUUID major identifier =
  sendOwnedMessage clBeaconRegion initWithProximityUUID_major_identifierSelector (toNSUUID proximityUUID) major (toNSString identifier)

-- | @- initWithUUID:major:minor:identifier:@
initWithUUID_major_minor_identifier :: (IsCLBeaconRegion clBeaconRegion, IsNSUUID uuid, IsNSString identifier) => clBeaconRegion -> uuid -> CUShort -> CUShort -> identifier -> IO (Id CLBeaconRegion)
initWithUUID_major_minor_identifier clBeaconRegion uuid major minor identifier =
  sendOwnedMessage clBeaconRegion initWithUUID_major_minor_identifierSelector (toNSUUID uuid) major minor (toNSString identifier)

-- | @- initWithProximityUUID:major:minor:identifier:@
initWithProximityUUID_major_minor_identifier :: (IsCLBeaconRegion clBeaconRegion, IsNSUUID proximityUUID, IsNSString identifier) => clBeaconRegion -> proximityUUID -> CUShort -> CUShort -> identifier -> IO (Id CLBeaconRegion)
initWithProximityUUID_major_minor_identifier clBeaconRegion proximityUUID major minor identifier =
  sendOwnedMessage clBeaconRegion initWithProximityUUID_major_minor_identifierSelector (toNSUUID proximityUUID) major minor (toNSString identifier)

-- | @- initWithBeaconIdentityConstraint:identifier:@
initWithBeaconIdentityConstraint_identifier :: (IsCLBeaconRegion clBeaconRegion, IsCLBeaconIdentityConstraint beaconIdentityConstraint, IsNSString identifier) => clBeaconRegion -> beaconIdentityConstraint -> identifier -> IO (Id CLBeaconRegion)
initWithBeaconIdentityConstraint_identifier clBeaconRegion beaconIdentityConstraint identifier =
  sendOwnedMessage clBeaconRegion initWithBeaconIdentityConstraint_identifierSelector (toCLBeaconIdentityConstraint beaconIdentityConstraint) (toNSString identifier)

-- | @- peripheralDataWithMeasuredPower:@
peripheralDataWithMeasuredPower :: (IsCLBeaconRegion clBeaconRegion, IsNSNumber measuredPower) => clBeaconRegion -> measuredPower -> IO (Id NSMutableDictionary)
peripheralDataWithMeasuredPower clBeaconRegion measuredPower =
  sendMessage clBeaconRegion peripheralDataWithMeasuredPowerSelector (toNSNumber measuredPower)

-- | @- beaconIdentityConstraint@
beaconIdentityConstraint :: IsCLBeaconRegion clBeaconRegion => clBeaconRegion -> IO (Id CLBeaconIdentityConstraint)
beaconIdentityConstraint clBeaconRegion =
  sendMessage clBeaconRegion beaconIdentityConstraintSelector

-- | @- UUID@
uuid :: IsCLBeaconRegion clBeaconRegion => clBeaconRegion -> IO (Id NSUUID)
uuid clBeaconRegion =
  sendMessage clBeaconRegion uuidSelector

-- | @- proximityUUID@
proximityUUID :: IsCLBeaconRegion clBeaconRegion => clBeaconRegion -> IO (Id NSUUID)
proximityUUID clBeaconRegion =
  sendMessage clBeaconRegion proximityUUIDSelector

-- | @- major@
major :: IsCLBeaconRegion clBeaconRegion => clBeaconRegion -> IO (Id NSNumber)
major clBeaconRegion =
  sendMessage clBeaconRegion majorSelector

-- | @- minor@
minor :: IsCLBeaconRegion clBeaconRegion => clBeaconRegion -> IO (Id NSNumber)
minor clBeaconRegion =
  sendMessage clBeaconRegion minorSelector

-- | @- notifyEntryStateOnDisplay@
notifyEntryStateOnDisplay :: IsCLBeaconRegion clBeaconRegion => clBeaconRegion -> IO Bool
notifyEntryStateOnDisplay clBeaconRegion =
  sendMessage clBeaconRegion notifyEntryStateOnDisplaySelector

-- | @- setNotifyEntryStateOnDisplay:@
setNotifyEntryStateOnDisplay :: IsCLBeaconRegion clBeaconRegion => clBeaconRegion -> Bool -> IO ()
setNotifyEntryStateOnDisplay clBeaconRegion value =
  sendMessage clBeaconRegion setNotifyEntryStateOnDisplaySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUUID:identifier:@
initWithUUID_identifierSelector :: Selector '[Id NSUUID, Id NSString] (Id CLBeaconRegion)
initWithUUID_identifierSelector = mkSelector "initWithUUID:identifier:"

-- | @Selector@ for @initWithProximityUUID:identifier:@
initWithProximityUUID_identifierSelector :: Selector '[Id NSUUID, Id NSString] (Id CLBeaconRegion)
initWithProximityUUID_identifierSelector = mkSelector "initWithProximityUUID:identifier:"

-- | @Selector@ for @initWithUUID:major:identifier:@
initWithUUID_major_identifierSelector :: Selector '[Id NSUUID, CUShort, Id NSString] (Id CLBeaconRegion)
initWithUUID_major_identifierSelector = mkSelector "initWithUUID:major:identifier:"

-- | @Selector@ for @initWithProximityUUID:major:identifier:@
initWithProximityUUID_major_identifierSelector :: Selector '[Id NSUUID, CUShort, Id NSString] (Id CLBeaconRegion)
initWithProximityUUID_major_identifierSelector = mkSelector "initWithProximityUUID:major:identifier:"

-- | @Selector@ for @initWithUUID:major:minor:identifier:@
initWithUUID_major_minor_identifierSelector :: Selector '[Id NSUUID, CUShort, CUShort, Id NSString] (Id CLBeaconRegion)
initWithUUID_major_minor_identifierSelector = mkSelector "initWithUUID:major:minor:identifier:"

-- | @Selector@ for @initWithProximityUUID:major:minor:identifier:@
initWithProximityUUID_major_minor_identifierSelector :: Selector '[Id NSUUID, CUShort, CUShort, Id NSString] (Id CLBeaconRegion)
initWithProximityUUID_major_minor_identifierSelector = mkSelector "initWithProximityUUID:major:minor:identifier:"

-- | @Selector@ for @initWithBeaconIdentityConstraint:identifier:@
initWithBeaconIdentityConstraint_identifierSelector :: Selector '[Id CLBeaconIdentityConstraint, Id NSString] (Id CLBeaconRegion)
initWithBeaconIdentityConstraint_identifierSelector = mkSelector "initWithBeaconIdentityConstraint:identifier:"

-- | @Selector@ for @peripheralDataWithMeasuredPower:@
peripheralDataWithMeasuredPowerSelector :: Selector '[Id NSNumber] (Id NSMutableDictionary)
peripheralDataWithMeasuredPowerSelector = mkSelector "peripheralDataWithMeasuredPower:"

-- | @Selector@ for @beaconIdentityConstraint@
beaconIdentityConstraintSelector :: Selector '[] (Id CLBeaconIdentityConstraint)
beaconIdentityConstraintSelector = mkSelector "beaconIdentityConstraint"

-- | @Selector@ for @UUID@
uuidSelector :: Selector '[] (Id NSUUID)
uuidSelector = mkSelector "UUID"

-- | @Selector@ for @proximityUUID@
proximityUUIDSelector :: Selector '[] (Id NSUUID)
proximityUUIDSelector = mkSelector "proximityUUID"

-- | @Selector@ for @major@
majorSelector :: Selector '[] (Id NSNumber)
majorSelector = mkSelector "major"

-- | @Selector@ for @minor@
minorSelector :: Selector '[] (Id NSNumber)
minorSelector = mkSelector "minor"

-- | @Selector@ for @notifyEntryStateOnDisplay@
notifyEntryStateOnDisplaySelector :: Selector '[] Bool
notifyEntryStateOnDisplaySelector = mkSelector "notifyEntryStateOnDisplay"

-- | @Selector@ for @setNotifyEntryStateOnDisplay:@
setNotifyEntryStateOnDisplaySelector :: Selector '[Bool] ()
setNotifyEntryStateOnDisplaySelector = mkSelector "setNotifyEntryStateOnDisplay:"

