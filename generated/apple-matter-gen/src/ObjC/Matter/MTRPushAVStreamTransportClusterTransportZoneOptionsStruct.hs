{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterTransportZoneOptionsStruct@.
module ObjC.Matter.MTRPushAVStreamTransportClusterTransportZoneOptionsStruct
  ( MTRPushAVStreamTransportClusterTransportZoneOptionsStruct
  , IsMTRPushAVStreamTransportClusterTransportZoneOptionsStruct(..)
  , zone
  , setZone
  , sensitivity
  , setSensitivity
  , sensitivitySelector
  , setSensitivitySelector
  , setZoneSelector
  , zoneSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- zone@
zone :: IsMTRPushAVStreamTransportClusterTransportZoneOptionsStruct mtrPushAVStreamTransportClusterTransportZoneOptionsStruct => mtrPushAVStreamTransportClusterTransportZoneOptionsStruct -> IO (Id NSNumber)
zone mtrPushAVStreamTransportClusterTransportZoneOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterTransportZoneOptionsStruct zoneSelector

-- | @- setZone:@
setZone :: (IsMTRPushAVStreamTransportClusterTransportZoneOptionsStruct mtrPushAVStreamTransportClusterTransportZoneOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportZoneOptionsStruct -> value -> IO ()
setZone mtrPushAVStreamTransportClusterTransportZoneOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportZoneOptionsStruct setZoneSelector (toNSNumber value)

-- | @- sensitivity@
sensitivity :: IsMTRPushAVStreamTransportClusterTransportZoneOptionsStruct mtrPushAVStreamTransportClusterTransportZoneOptionsStruct => mtrPushAVStreamTransportClusterTransportZoneOptionsStruct -> IO (Id NSNumber)
sensitivity mtrPushAVStreamTransportClusterTransportZoneOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterTransportZoneOptionsStruct sensitivitySelector

-- | @- setSensitivity:@
setSensitivity :: (IsMTRPushAVStreamTransportClusterTransportZoneOptionsStruct mtrPushAVStreamTransportClusterTransportZoneOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportZoneOptionsStruct -> value -> IO ()
setSensitivity mtrPushAVStreamTransportClusterTransportZoneOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportZoneOptionsStruct setSensitivitySelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @zone@
zoneSelector :: Selector '[] (Id NSNumber)
zoneSelector = mkSelector "zone"

-- | @Selector@ for @setZone:@
setZoneSelector :: Selector '[Id NSNumber] ()
setZoneSelector = mkSelector "setZone:"

-- | @Selector@ for @sensitivity@
sensitivitySelector :: Selector '[] (Id NSNumber)
sensitivitySelector = mkSelector "sensitivity"

-- | @Selector@ for @setSensitivity:@
setSensitivitySelector :: Selector '[Id NSNumber] ()
setSensitivitySelector = mkSelector "setSensitivity:"

