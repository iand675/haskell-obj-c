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
  , major
  , minor
  , notifyEntryStateOnDisplay
  , setNotifyEntryStateOnDisplay
  , initWithUUID_identifierSelector
  , initWithProximityUUID_identifierSelector
  , initWithUUID_major_identifierSelector
  , initWithProximityUUID_major_identifierSelector
  , initWithUUID_major_minor_identifierSelector
  , initWithProximityUUID_major_minor_identifierSelector
  , initWithBeaconIdentityConstraint_identifierSelector
  , peripheralDataWithMeasuredPowerSelector
  , beaconIdentityConstraintSelector
  , uuidSelector
  , majorSelector
  , minorSelector
  , notifyEntryStateOnDisplaySelector
  , setNotifyEntryStateOnDisplaySelector


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

import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithUUID:identifier:@
initWithUUID_identifier :: (IsCLBeaconRegion clBeaconRegion, IsNSUUID uuid, IsNSString identifier) => clBeaconRegion -> uuid -> identifier -> IO (Id CLBeaconRegion)
initWithUUID_identifier clBeaconRegion  uuid identifier =
withObjCPtr uuid $ \raw_uuid ->
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg clBeaconRegion (mkSelector "initWithUUID:identifier:") (retPtr retVoid) [argPtr (castPtr raw_uuid :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithProximityUUID:identifier:@
initWithProximityUUID_identifier :: (IsCLBeaconRegion clBeaconRegion, IsNSUUID proximityUUID, IsNSString identifier) => clBeaconRegion -> proximityUUID -> identifier -> IO (Id CLBeaconRegion)
initWithProximityUUID_identifier clBeaconRegion  proximityUUID identifier =
withObjCPtr proximityUUID $ \raw_proximityUUID ->
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg clBeaconRegion (mkSelector "initWithProximityUUID:identifier:") (retPtr retVoid) [argPtr (castPtr raw_proximityUUID :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithUUID:major:identifier:@
initWithUUID_major_identifier :: (IsCLBeaconRegion clBeaconRegion, IsNSUUID uuid, IsNSString identifier) => clBeaconRegion -> uuid -> CUShort -> identifier -> IO (Id CLBeaconRegion)
initWithUUID_major_identifier clBeaconRegion  uuid major identifier =
withObjCPtr uuid $ \raw_uuid ->
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg clBeaconRegion (mkSelector "initWithUUID:major:identifier:") (retPtr retVoid) [argPtr (castPtr raw_uuid :: Ptr ()), argCUInt (fromIntegral major), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithProximityUUID:major:identifier:@
initWithProximityUUID_major_identifier :: (IsCLBeaconRegion clBeaconRegion, IsNSUUID proximityUUID, IsNSString identifier) => clBeaconRegion -> proximityUUID -> CUShort -> identifier -> IO (Id CLBeaconRegion)
initWithProximityUUID_major_identifier clBeaconRegion  proximityUUID major identifier =
withObjCPtr proximityUUID $ \raw_proximityUUID ->
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg clBeaconRegion (mkSelector "initWithProximityUUID:major:identifier:") (retPtr retVoid) [argPtr (castPtr raw_proximityUUID :: Ptr ()), argCUInt (fromIntegral major), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithUUID:major:minor:identifier:@
initWithUUID_major_minor_identifier :: (IsCLBeaconRegion clBeaconRegion, IsNSUUID uuid, IsNSString identifier) => clBeaconRegion -> uuid -> CUShort -> CUShort -> identifier -> IO (Id CLBeaconRegion)
initWithUUID_major_minor_identifier clBeaconRegion  uuid major minor identifier =
withObjCPtr uuid $ \raw_uuid ->
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg clBeaconRegion (mkSelector "initWithUUID:major:minor:identifier:") (retPtr retVoid) [argPtr (castPtr raw_uuid :: Ptr ()), argCUInt (fromIntegral major), argCUInt (fromIntegral minor), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithProximityUUID:major:minor:identifier:@
initWithProximityUUID_major_minor_identifier :: (IsCLBeaconRegion clBeaconRegion, IsNSUUID proximityUUID, IsNSString identifier) => clBeaconRegion -> proximityUUID -> CUShort -> CUShort -> identifier -> IO (Id CLBeaconRegion)
initWithProximityUUID_major_minor_identifier clBeaconRegion  proximityUUID major minor identifier =
withObjCPtr proximityUUID $ \raw_proximityUUID ->
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg clBeaconRegion (mkSelector "initWithProximityUUID:major:minor:identifier:") (retPtr retVoid) [argPtr (castPtr raw_proximityUUID :: Ptr ()), argCUInt (fromIntegral major), argCUInt (fromIntegral minor), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithBeaconIdentityConstraint:identifier:@
initWithBeaconIdentityConstraint_identifier :: (IsCLBeaconRegion clBeaconRegion, IsCLBeaconIdentityConstraint beaconIdentityConstraint, IsNSString identifier) => clBeaconRegion -> beaconIdentityConstraint -> identifier -> IO (Id CLBeaconRegion)
initWithBeaconIdentityConstraint_identifier clBeaconRegion  beaconIdentityConstraint identifier =
withObjCPtr beaconIdentityConstraint $ \raw_beaconIdentityConstraint ->
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg clBeaconRegion (mkSelector "initWithBeaconIdentityConstraint:identifier:") (retPtr retVoid) [argPtr (castPtr raw_beaconIdentityConstraint :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- peripheralDataWithMeasuredPower:@
peripheralDataWithMeasuredPower :: (IsCLBeaconRegion clBeaconRegion, IsNSNumber measuredPower) => clBeaconRegion -> measuredPower -> IO (Id NSMutableDictionary)
peripheralDataWithMeasuredPower clBeaconRegion  measuredPower =
withObjCPtr measuredPower $ \raw_measuredPower ->
    sendMsg clBeaconRegion (mkSelector "peripheralDataWithMeasuredPower:") (retPtr retVoid) [argPtr (castPtr raw_measuredPower :: Ptr ())] >>= retainedObject . castPtr

-- | @- beaconIdentityConstraint@
beaconIdentityConstraint :: IsCLBeaconRegion clBeaconRegion => clBeaconRegion -> IO (Id CLBeaconIdentityConstraint)
beaconIdentityConstraint clBeaconRegion  =
  sendMsg clBeaconRegion (mkSelector "beaconIdentityConstraint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- UUID@
uuid :: IsCLBeaconRegion clBeaconRegion => clBeaconRegion -> IO (Id NSUUID)
uuid clBeaconRegion  =
  sendMsg clBeaconRegion (mkSelector "UUID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- major@
major :: IsCLBeaconRegion clBeaconRegion => clBeaconRegion -> IO (Id NSNumber)
major clBeaconRegion  =
  sendMsg clBeaconRegion (mkSelector "major") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- minor@
minor :: IsCLBeaconRegion clBeaconRegion => clBeaconRegion -> IO (Id NSNumber)
minor clBeaconRegion  =
  sendMsg clBeaconRegion (mkSelector "minor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- notifyEntryStateOnDisplay@
notifyEntryStateOnDisplay :: IsCLBeaconRegion clBeaconRegion => clBeaconRegion -> IO Bool
notifyEntryStateOnDisplay clBeaconRegion  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clBeaconRegion (mkSelector "notifyEntryStateOnDisplay") retCULong []

-- | @- setNotifyEntryStateOnDisplay:@
setNotifyEntryStateOnDisplay :: IsCLBeaconRegion clBeaconRegion => clBeaconRegion -> Bool -> IO ()
setNotifyEntryStateOnDisplay clBeaconRegion  value =
  sendMsg clBeaconRegion (mkSelector "setNotifyEntryStateOnDisplay:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUUID:identifier:@
initWithUUID_identifierSelector :: Selector
initWithUUID_identifierSelector = mkSelector "initWithUUID:identifier:"

-- | @Selector@ for @initWithProximityUUID:identifier:@
initWithProximityUUID_identifierSelector :: Selector
initWithProximityUUID_identifierSelector = mkSelector "initWithProximityUUID:identifier:"

-- | @Selector@ for @initWithUUID:major:identifier:@
initWithUUID_major_identifierSelector :: Selector
initWithUUID_major_identifierSelector = mkSelector "initWithUUID:major:identifier:"

-- | @Selector@ for @initWithProximityUUID:major:identifier:@
initWithProximityUUID_major_identifierSelector :: Selector
initWithProximityUUID_major_identifierSelector = mkSelector "initWithProximityUUID:major:identifier:"

-- | @Selector@ for @initWithUUID:major:minor:identifier:@
initWithUUID_major_minor_identifierSelector :: Selector
initWithUUID_major_minor_identifierSelector = mkSelector "initWithUUID:major:minor:identifier:"

-- | @Selector@ for @initWithProximityUUID:major:minor:identifier:@
initWithProximityUUID_major_minor_identifierSelector :: Selector
initWithProximityUUID_major_minor_identifierSelector = mkSelector "initWithProximityUUID:major:minor:identifier:"

-- | @Selector@ for @initWithBeaconIdentityConstraint:identifier:@
initWithBeaconIdentityConstraint_identifierSelector :: Selector
initWithBeaconIdentityConstraint_identifierSelector = mkSelector "initWithBeaconIdentityConstraint:identifier:"

-- | @Selector@ for @peripheralDataWithMeasuredPower:@
peripheralDataWithMeasuredPowerSelector :: Selector
peripheralDataWithMeasuredPowerSelector = mkSelector "peripheralDataWithMeasuredPower:"

-- | @Selector@ for @beaconIdentityConstraint@
beaconIdentityConstraintSelector :: Selector
beaconIdentityConstraintSelector = mkSelector "beaconIdentityConstraint"

-- | @Selector@ for @UUID@
uuidSelector :: Selector
uuidSelector = mkSelector "UUID"

-- | @Selector@ for @major@
majorSelector :: Selector
majorSelector = mkSelector "major"

-- | @Selector@ for @minor@
minorSelector :: Selector
minorSelector = mkSelector "minor"

-- | @Selector@ for @notifyEntryStateOnDisplay@
notifyEntryStateOnDisplaySelector :: Selector
notifyEntryStateOnDisplaySelector = mkSelector "notifyEntryStateOnDisplay"

-- | @Selector@ for @setNotifyEntryStateOnDisplay:@
setNotifyEntryStateOnDisplaySelector :: Selector
setNotifyEntryStateOnDisplaySelector = mkSelector "setNotifyEntryStateOnDisplay:"

