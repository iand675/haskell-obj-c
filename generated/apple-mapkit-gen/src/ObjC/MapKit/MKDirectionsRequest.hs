{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKDirectionsRequest@.
module ObjC.MapKit.MKDirectionsRequest
  ( MKDirectionsRequest
  , IsMKDirectionsRequest(..)
  , setSource
  , setDestination
  , initWithContentsOfURL
  , isDirectionsRequestURL
  , source
  , destination
  , transportType
  , setTransportType
  , requestsAlternateRoutes
  , setRequestsAlternateRoutes
  , departureDate
  , setDepartureDate
  , arrivalDate
  , setArrivalDate
  , tollPreference
  , setTollPreference
  , highwayPreference
  , setHighwayPreference
  , arrivalDateSelector
  , departureDateSelector
  , destinationSelector
  , highwayPreferenceSelector
  , initWithContentsOfURLSelector
  , isDirectionsRequestURLSelector
  , requestsAlternateRoutesSelector
  , setArrivalDateSelector
  , setDepartureDateSelector
  , setDestinationSelector
  , setHighwayPreferenceSelector
  , setRequestsAlternateRoutesSelector
  , setSourceSelector
  , setTollPreferenceSelector
  , setTransportTypeSelector
  , sourceSelector
  , tollPreferenceSelector
  , transportTypeSelector

  -- * Enum types
  , MKDirectionsRoutePreference(MKDirectionsRoutePreference)
  , pattern MKDirectionsRoutePreferenceAny
  , pattern MKDirectionsRoutePreferenceAvoid
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

-- | @- setSource:@
setSource :: (IsMKDirectionsRequest mkDirectionsRequest, IsMKMapItem source) => mkDirectionsRequest -> source -> IO ()
setSource mkDirectionsRequest source =
  sendMessage mkDirectionsRequest setSourceSelector (toMKMapItem source)

-- | @- setDestination:@
setDestination :: (IsMKDirectionsRequest mkDirectionsRequest, IsMKMapItem destination) => mkDirectionsRequest -> destination -> IO ()
setDestination mkDirectionsRequest destination =
  sendMessage mkDirectionsRequest setDestinationSelector (toMKMapItem destination)

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsMKDirectionsRequest mkDirectionsRequest, IsNSURL url) => mkDirectionsRequest -> url -> IO (Id MKDirectionsRequest)
initWithContentsOfURL mkDirectionsRequest url =
  sendOwnedMessage mkDirectionsRequest initWithContentsOfURLSelector (toNSURL url)

-- | @+ isDirectionsRequestURL:@
isDirectionsRequestURL :: IsNSURL url => url -> IO Bool
isDirectionsRequestURL url =
  do
    cls' <- getRequiredClass "MKDirectionsRequest"
    sendClassMessage cls' isDirectionsRequestURLSelector (toNSURL url)

-- | @- source@
source :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> IO (Id MKMapItem)
source mkDirectionsRequest =
  sendMessage mkDirectionsRequest sourceSelector

-- | @- destination@
destination :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> IO (Id MKMapItem)
destination mkDirectionsRequest =
  sendMessage mkDirectionsRequest destinationSelector

-- | @- transportType@
transportType :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> IO MKDirectionsTransportType
transportType mkDirectionsRequest =
  sendMessage mkDirectionsRequest transportTypeSelector

-- | @- setTransportType:@
setTransportType :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> MKDirectionsTransportType -> IO ()
setTransportType mkDirectionsRequest value =
  sendMessage mkDirectionsRequest setTransportTypeSelector value

-- | @- requestsAlternateRoutes@
requestsAlternateRoutes :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> IO Bool
requestsAlternateRoutes mkDirectionsRequest =
  sendMessage mkDirectionsRequest requestsAlternateRoutesSelector

-- | @- setRequestsAlternateRoutes:@
setRequestsAlternateRoutes :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> Bool -> IO ()
setRequestsAlternateRoutes mkDirectionsRequest value =
  sendMessage mkDirectionsRequest setRequestsAlternateRoutesSelector value

-- | @- departureDate@
departureDate :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> IO RawId
departureDate mkDirectionsRequest =
  sendMessage mkDirectionsRequest departureDateSelector

-- | @- setDepartureDate:@
setDepartureDate :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> RawId -> IO ()
setDepartureDate mkDirectionsRequest value =
  sendMessage mkDirectionsRequest setDepartureDateSelector value

-- | @- arrivalDate@
arrivalDate :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> IO RawId
arrivalDate mkDirectionsRequest =
  sendMessage mkDirectionsRequest arrivalDateSelector

-- | @- setArrivalDate:@
setArrivalDate :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> RawId -> IO ()
setArrivalDate mkDirectionsRequest value =
  sendMessage mkDirectionsRequest setArrivalDateSelector value

-- | @- tollPreference@
tollPreference :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> IO MKDirectionsRoutePreference
tollPreference mkDirectionsRequest =
  sendMessage mkDirectionsRequest tollPreferenceSelector

-- | @- setTollPreference:@
setTollPreference :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> MKDirectionsRoutePreference -> IO ()
setTollPreference mkDirectionsRequest value =
  sendMessage mkDirectionsRequest setTollPreferenceSelector value

-- | @- highwayPreference@
highwayPreference :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> IO MKDirectionsRoutePreference
highwayPreference mkDirectionsRequest =
  sendMessage mkDirectionsRequest highwayPreferenceSelector

-- | @- setHighwayPreference:@
setHighwayPreference :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> MKDirectionsRoutePreference -> IO ()
setHighwayPreference mkDirectionsRequest value =
  sendMessage mkDirectionsRequest setHighwayPreferenceSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setSource:@
setSourceSelector :: Selector '[Id MKMapItem] ()
setSourceSelector = mkSelector "setSource:"

-- | @Selector@ for @setDestination:@
setDestinationSelector :: Selector '[Id MKMapItem] ()
setDestinationSelector = mkSelector "setDestination:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector '[Id NSURL] (Id MKDirectionsRequest)
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @isDirectionsRequestURL:@
isDirectionsRequestURLSelector :: Selector '[Id NSURL] Bool
isDirectionsRequestURLSelector = mkSelector "isDirectionsRequestURL:"

-- | @Selector@ for @source@
sourceSelector :: Selector '[] (Id MKMapItem)
sourceSelector = mkSelector "source"

-- | @Selector@ for @destination@
destinationSelector :: Selector '[] (Id MKMapItem)
destinationSelector = mkSelector "destination"

-- | @Selector@ for @transportType@
transportTypeSelector :: Selector '[] MKDirectionsTransportType
transportTypeSelector = mkSelector "transportType"

-- | @Selector@ for @setTransportType:@
setTransportTypeSelector :: Selector '[MKDirectionsTransportType] ()
setTransportTypeSelector = mkSelector "setTransportType:"

-- | @Selector@ for @requestsAlternateRoutes@
requestsAlternateRoutesSelector :: Selector '[] Bool
requestsAlternateRoutesSelector = mkSelector "requestsAlternateRoutes"

-- | @Selector@ for @setRequestsAlternateRoutes:@
setRequestsAlternateRoutesSelector :: Selector '[Bool] ()
setRequestsAlternateRoutesSelector = mkSelector "setRequestsAlternateRoutes:"

-- | @Selector@ for @departureDate@
departureDateSelector :: Selector '[] RawId
departureDateSelector = mkSelector "departureDate"

-- | @Selector@ for @setDepartureDate:@
setDepartureDateSelector :: Selector '[RawId] ()
setDepartureDateSelector = mkSelector "setDepartureDate:"

-- | @Selector@ for @arrivalDate@
arrivalDateSelector :: Selector '[] RawId
arrivalDateSelector = mkSelector "arrivalDate"

-- | @Selector@ for @setArrivalDate:@
setArrivalDateSelector :: Selector '[RawId] ()
setArrivalDateSelector = mkSelector "setArrivalDate:"

-- | @Selector@ for @tollPreference@
tollPreferenceSelector :: Selector '[] MKDirectionsRoutePreference
tollPreferenceSelector = mkSelector "tollPreference"

-- | @Selector@ for @setTollPreference:@
setTollPreferenceSelector :: Selector '[MKDirectionsRoutePreference] ()
setTollPreferenceSelector = mkSelector "setTollPreference:"

-- | @Selector@ for @highwayPreference@
highwayPreferenceSelector :: Selector '[] MKDirectionsRoutePreference
highwayPreferenceSelector = mkSelector "highwayPreference"

-- | @Selector@ for @setHighwayPreference:@
setHighwayPreferenceSelector :: Selector '[MKDirectionsRoutePreference] ()
setHighwayPreferenceSelector = mkSelector "setHighwayPreference:"

