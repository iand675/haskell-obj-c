{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLBeacon@.
module ObjC.CoreLocation.CLBeacon
  ( CLBeacon
  , IsCLBeacon(..)
  , timestamp
  , uuid
  , proximityUUID
  , major
  , minor
  , proximity
  , accuracy
  , rssi
  , accuracySelector
  , majorSelector
  , minorSelector
  , proximitySelector
  , proximityUUIDSelector
  , rssiSelector
  , timestampSelector
  , uuidSelector

  -- * Enum types
  , CLProximity(CLProximity)
  , pattern CLProximityUnknown
  , pattern CLProximityImmediate
  , pattern CLProximityNear
  , pattern CLProximityFar

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreLocation.Internal.Classes
import ObjC.CoreLocation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- timestamp@
timestamp :: IsCLBeacon clBeacon => clBeacon -> IO (Id NSDate)
timestamp clBeacon =
  sendMessage clBeacon timestampSelector

-- | @- UUID@
uuid :: IsCLBeacon clBeacon => clBeacon -> IO (Id NSUUID)
uuid clBeacon =
  sendMessage clBeacon uuidSelector

-- | @- proximityUUID@
proximityUUID :: IsCLBeacon clBeacon => clBeacon -> IO (Id NSUUID)
proximityUUID clBeacon =
  sendMessage clBeacon proximityUUIDSelector

-- | @- major@
major :: IsCLBeacon clBeacon => clBeacon -> IO (Id NSNumber)
major clBeacon =
  sendMessage clBeacon majorSelector

-- | @- minor@
minor :: IsCLBeacon clBeacon => clBeacon -> IO (Id NSNumber)
minor clBeacon =
  sendMessage clBeacon minorSelector

-- | @- proximity@
proximity :: IsCLBeacon clBeacon => clBeacon -> IO CLProximity
proximity clBeacon =
  sendMessage clBeacon proximitySelector

-- | @- accuracy@
accuracy :: IsCLBeacon clBeacon => clBeacon -> IO CDouble
accuracy clBeacon =
  sendMessage clBeacon accuracySelector

-- | @- rssi@
rssi :: IsCLBeacon clBeacon => clBeacon -> IO CLong
rssi clBeacon =
  sendMessage clBeacon rssiSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @timestamp@
timestampSelector :: Selector '[] (Id NSDate)
timestampSelector = mkSelector "timestamp"

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

-- | @Selector@ for @proximity@
proximitySelector :: Selector '[] CLProximity
proximitySelector = mkSelector "proximity"

-- | @Selector@ for @accuracy@
accuracySelector :: Selector '[] CDouble
accuracySelector = mkSelector "accuracy"

-- | @Selector@ for @rssi@
rssiSelector :: Selector '[] CLong
rssiSelector = mkSelector "rssi"

