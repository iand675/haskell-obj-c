{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLBeaconIdentityCondition@.
module ObjC.CoreLocation.CLBeaconIdentityCondition
  ( CLBeaconIdentityCondition
  , IsCLBeaconIdentityCondition(..)
  , initWithUUID
  , initWithUUID_major
  , initWithUUID_major_minor
  , uuid
  , major
  , minor
  , initWithUUIDSelector
  , initWithUUID_majorSelector
  , initWithUUID_major_minorSelector
  , majorSelector
  , minorSelector
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

-- | @- initWithUUID:@
initWithUUID :: (IsCLBeaconIdentityCondition clBeaconIdentityCondition, IsNSUUID uuid) => clBeaconIdentityCondition -> uuid -> IO (Id CLBeaconIdentityCondition)
initWithUUID clBeaconIdentityCondition uuid =
  sendOwnedMessage clBeaconIdentityCondition initWithUUIDSelector (toNSUUID uuid)

-- | @- initWithUUID:major:@
initWithUUID_major :: (IsCLBeaconIdentityCondition clBeaconIdentityCondition, IsNSUUID uuid) => clBeaconIdentityCondition -> uuid -> CUShort -> IO (Id CLBeaconIdentityCondition)
initWithUUID_major clBeaconIdentityCondition uuid major =
  sendOwnedMessage clBeaconIdentityCondition initWithUUID_majorSelector (toNSUUID uuid) major

-- | @- initWithUUID:major:minor:@
initWithUUID_major_minor :: (IsCLBeaconIdentityCondition clBeaconIdentityCondition, IsNSUUID uuid) => clBeaconIdentityCondition -> uuid -> CUShort -> CUShort -> IO (Id CLBeaconIdentityCondition)
initWithUUID_major_minor clBeaconIdentityCondition uuid major minor =
  sendOwnedMessage clBeaconIdentityCondition initWithUUID_major_minorSelector (toNSUUID uuid) major minor

-- | @- UUID@
uuid :: IsCLBeaconIdentityCondition clBeaconIdentityCondition => clBeaconIdentityCondition -> IO (Id NSUUID)
uuid clBeaconIdentityCondition =
  sendMessage clBeaconIdentityCondition uuidSelector

-- | @- major@
major :: IsCLBeaconIdentityCondition clBeaconIdentityCondition => clBeaconIdentityCondition -> IO (Id NSNumber)
major clBeaconIdentityCondition =
  sendMessage clBeaconIdentityCondition majorSelector

-- | @- minor@
minor :: IsCLBeaconIdentityCondition clBeaconIdentityCondition => clBeaconIdentityCondition -> IO (Id NSNumber)
minor clBeaconIdentityCondition =
  sendMessage clBeaconIdentityCondition minorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUUID:@
initWithUUIDSelector :: Selector '[Id NSUUID] (Id CLBeaconIdentityCondition)
initWithUUIDSelector = mkSelector "initWithUUID:"

-- | @Selector@ for @initWithUUID:major:@
initWithUUID_majorSelector :: Selector '[Id NSUUID, CUShort] (Id CLBeaconIdentityCondition)
initWithUUID_majorSelector = mkSelector "initWithUUID:major:"

-- | @Selector@ for @initWithUUID:major:minor:@
initWithUUID_major_minorSelector :: Selector '[Id NSUUID, CUShort, CUShort] (Id CLBeaconIdentityCondition)
initWithUUID_major_minorSelector = mkSelector "initWithUUID:major:minor:"

-- | @Selector@ for @UUID@
uuidSelector :: Selector '[] (Id NSUUID)
uuidSelector = mkSelector "UUID"

-- | @Selector@ for @major@
majorSelector :: Selector '[] (Id NSNumber)
majorSelector = mkSelector "major"

-- | @Selector@ for @minor@
minorSelector :: Selector '[] (Id NSNumber)
minorSelector = mkSelector "minor"

