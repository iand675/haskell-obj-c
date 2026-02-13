{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , destinationSelector
  , distanceSelector
  , expectedArrivalDateSelector
  , expectedDepartureDateSelector
  , expectedTravelTimeSelector
  , sourceSelector
  , transportTypeSelector

  -- * Enum types
  , MKDirectionsTransportType(MKDirectionsTransportType)
  , pattern MKDirectionsTransportTypeAutomobile
  , pattern MKDirectionsTransportTypeWalking
  , pattern MKDirectionsTransportTypeTransit
  , pattern MKDirectionsTransportTypeCycling
  , pattern MKDirectionsTransportTypeAny

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.MapKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- source@
source :: IsMKETAResponse mketaResponse => mketaResponse -> IO (Id MKMapItem)
source mketaResponse =
  sendMessage mketaResponse sourceSelector

-- | @- destination@
destination :: IsMKETAResponse mketaResponse => mketaResponse -> IO (Id MKMapItem)
destination mketaResponse =
  sendMessage mketaResponse destinationSelector

-- | @- expectedTravelTime@
expectedTravelTime :: IsMKETAResponse mketaResponse => mketaResponse -> IO CDouble
expectedTravelTime mketaResponse =
  sendMessage mketaResponse expectedTravelTimeSelector

-- | @- distance@
distance :: IsMKETAResponse mketaResponse => mketaResponse -> IO CDouble
distance mketaResponse =
  sendMessage mketaResponse distanceSelector

-- | @- expectedArrivalDate@
expectedArrivalDate :: IsMKETAResponse mketaResponse => mketaResponse -> IO RawId
expectedArrivalDate mketaResponse =
  sendMessage mketaResponse expectedArrivalDateSelector

-- | @- expectedDepartureDate@
expectedDepartureDate :: IsMKETAResponse mketaResponse => mketaResponse -> IO RawId
expectedDepartureDate mketaResponse =
  sendMessage mketaResponse expectedDepartureDateSelector

-- | @- transportType@
transportType :: IsMKETAResponse mketaResponse => mketaResponse -> IO MKDirectionsTransportType
transportType mketaResponse =
  sendMessage mketaResponse transportTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @source@
sourceSelector :: Selector '[] (Id MKMapItem)
sourceSelector = mkSelector "source"

-- | @Selector@ for @destination@
destinationSelector :: Selector '[] (Id MKMapItem)
destinationSelector = mkSelector "destination"

-- | @Selector@ for @expectedTravelTime@
expectedTravelTimeSelector :: Selector '[] CDouble
expectedTravelTimeSelector = mkSelector "expectedTravelTime"

-- | @Selector@ for @distance@
distanceSelector :: Selector '[] CDouble
distanceSelector = mkSelector "distance"

-- | @Selector@ for @expectedArrivalDate@
expectedArrivalDateSelector :: Selector '[] RawId
expectedArrivalDateSelector = mkSelector "expectedArrivalDate"

-- | @Selector@ for @expectedDepartureDate@
expectedDepartureDateSelector :: Selector '[] RawId
expectedDepartureDateSelector = mkSelector "expectedDepartureDate"

-- | @Selector@ for @transportType@
transportTypeSelector :: Selector '[] MKDirectionsTransportType
transportTypeSelector = mkSelector "transportType"

