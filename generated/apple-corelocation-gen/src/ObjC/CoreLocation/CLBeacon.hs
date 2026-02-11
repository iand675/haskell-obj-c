{-# LANGUAGE PatternSynonyms #-}
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
  , timestampSelector
  , uuidSelector
  , proximityUUIDSelector
  , majorSelector
  , minorSelector
  , proximitySelector
  , accuracySelector
  , rssiSelector

  -- * Enum types
  , CLProximity(CLProximity)
  , pattern CLProximityUnknown
  , pattern CLProximityImmediate
  , pattern CLProximityNear
  , pattern CLProximityFar

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
import ObjC.CoreLocation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- timestamp@
timestamp :: IsCLBeacon clBeacon => clBeacon -> IO (Id NSDate)
timestamp clBeacon  =
    sendMsg clBeacon (mkSelector "timestamp") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- UUID@
uuid :: IsCLBeacon clBeacon => clBeacon -> IO (Id NSUUID)
uuid clBeacon  =
    sendMsg clBeacon (mkSelector "UUID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- proximityUUID@
proximityUUID :: IsCLBeacon clBeacon => clBeacon -> IO (Id NSUUID)
proximityUUID clBeacon  =
    sendMsg clBeacon (mkSelector "proximityUUID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- major@
major :: IsCLBeacon clBeacon => clBeacon -> IO (Id NSNumber)
major clBeacon  =
    sendMsg clBeacon (mkSelector "major") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- minor@
minor :: IsCLBeacon clBeacon => clBeacon -> IO (Id NSNumber)
minor clBeacon  =
    sendMsg clBeacon (mkSelector "minor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- proximity@
proximity :: IsCLBeacon clBeacon => clBeacon -> IO CLProximity
proximity clBeacon  =
    fmap (coerce :: CLong -> CLProximity) $ sendMsg clBeacon (mkSelector "proximity") retCLong []

-- | @- accuracy@
accuracy :: IsCLBeacon clBeacon => clBeacon -> IO CDouble
accuracy clBeacon  =
    sendMsg clBeacon (mkSelector "accuracy") retCDouble []

-- | @- rssi@
rssi :: IsCLBeacon clBeacon => clBeacon -> IO CLong
rssi clBeacon  =
    sendMsg clBeacon (mkSelector "rssi") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @timestamp@
timestampSelector :: Selector
timestampSelector = mkSelector "timestamp"

-- | @Selector@ for @UUID@
uuidSelector :: Selector
uuidSelector = mkSelector "UUID"

-- | @Selector@ for @proximityUUID@
proximityUUIDSelector :: Selector
proximityUUIDSelector = mkSelector "proximityUUID"

-- | @Selector@ for @major@
majorSelector :: Selector
majorSelector = mkSelector "major"

-- | @Selector@ for @minor@
minorSelector :: Selector
minorSelector = mkSelector "minor"

-- | @Selector@ for @proximity@
proximitySelector :: Selector
proximitySelector = mkSelector "proximity"

-- | @Selector@ for @accuracy@
accuracySelector :: Selector
accuracySelector = mkSelector "accuracy"

-- | @Selector@ for @rssi@
rssiSelector :: Selector
rssiSelector = mkSelector "rssi"

