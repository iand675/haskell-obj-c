{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKETAResponse@.
module ObjC.MapKit.MKETAResponse
  ( MKETAResponse
  , IsMKETAResponse(..)
  , source
  , destination
  , expectedTravelTime
  , distance
  , expectedArrivalDate
  , expectedDepartureDate
  , transportType
  , sourceSelector
  , destinationSelector
  , expectedTravelTimeSelector
  , distanceSelector
  , expectedArrivalDateSelector
  , expectedDepartureDateSelector
  , transportTypeSelector

  -- * Enum types
  , MKDirectionsTransportType(MKDirectionsTransportType)
  , pattern MKDirectionsTransportTypeAutomobile
  , pattern MKDirectionsTransportTypeWalking
  , pattern MKDirectionsTransportTypeTransit
  , pattern MKDirectionsTransportTypeCycling
  , pattern MKDirectionsTransportTypeAny

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

import ObjC.MapKit.Internal.Classes
import ObjC.MapKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- source@
source :: IsMKETAResponse mketaResponse => mketaResponse -> IO (Id MKMapItem)
source mketaResponse  =
    sendMsg mketaResponse (mkSelector "source") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- destination@
destination :: IsMKETAResponse mketaResponse => mketaResponse -> IO (Id MKMapItem)
destination mketaResponse  =
    sendMsg mketaResponse (mkSelector "destination") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- expectedTravelTime@
expectedTravelTime :: IsMKETAResponse mketaResponse => mketaResponse -> IO CDouble
expectedTravelTime mketaResponse  =
    sendMsg mketaResponse (mkSelector "expectedTravelTime") retCDouble []

-- | @- distance@
distance :: IsMKETAResponse mketaResponse => mketaResponse -> IO CDouble
distance mketaResponse  =
    sendMsg mketaResponse (mkSelector "distance") retCDouble []

-- | @- expectedArrivalDate@
expectedArrivalDate :: IsMKETAResponse mketaResponse => mketaResponse -> IO RawId
expectedArrivalDate mketaResponse  =
    fmap (RawId . castPtr) $ sendMsg mketaResponse (mkSelector "expectedArrivalDate") (retPtr retVoid) []

-- | @- expectedDepartureDate@
expectedDepartureDate :: IsMKETAResponse mketaResponse => mketaResponse -> IO RawId
expectedDepartureDate mketaResponse  =
    fmap (RawId . castPtr) $ sendMsg mketaResponse (mkSelector "expectedDepartureDate") (retPtr retVoid) []

-- | @- transportType@
transportType :: IsMKETAResponse mketaResponse => mketaResponse -> IO MKDirectionsTransportType
transportType mketaResponse  =
    fmap (coerce :: CULong -> MKDirectionsTransportType) $ sendMsg mketaResponse (mkSelector "transportType") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @source@
sourceSelector :: Selector
sourceSelector = mkSelector "source"

-- | @Selector@ for @destination@
destinationSelector :: Selector
destinationSelector = mkSelector "destination"

-- | @Selector@ for @expectedTravelTime@
expectedTravelTimeSelector :: Selector
expectedTravelTimeSelector = mkSelector "expectedTravelTime"

-- | @Selector@ for @distance@
distanceSelector :: Selector
distanceSelector = mkSelector "distance"

-- | @Selector@ for @expectedArrivalDate@
expectedArrivalDateSelector :: Selector
expectedArrivalDateSelector = mkSelector "expectedArrivalDate"

-- | @Selector@ for @expectedDepartureDate@
expectedDepartureDateSelector :: Selector
expectedDepartureDateSelector = mkSelector "expectedDepartureDate"

-- | @Selector@ for @transportType@
transportTypeSelector :: Selector
transportTypeSelector = mkSelector "transportType"

