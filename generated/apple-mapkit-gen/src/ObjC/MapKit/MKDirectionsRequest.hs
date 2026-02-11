{-# LANGUAGE PatternSynonyms #-}
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
  , setSourceSelector
  , setDestinationSelector
  , initWithContentsOfURLSelector
  , isDirectionsRequestURLSelector
  , sourceSelector
  , destinationSelector
  , transportTypeSelector
  , setTransportTypeSelector
  , requestsAlternateRoutesSelector
  , setRequestsAlternateRoutesSelector
  , departureDateSelector
  , setDepartureDateSelector
  , arrivalDateSelector
  , setArrivalDateSelector
  , tollPreferenceSelector
  , setTollPreferenceSelector
  , highwayPreferenceSelector
  , setHighwayPreferenceSelector

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

-- | @- setSource:@
setSource :: (IsMKDirectionsRequest mkDirectionsRequest, IsMKMapItem source) => mkDirectionsRequest -> source -> IO ()
setSource mkDirectionsRequest  source =
  withObjCPtr source $ \raw_source ->
      sendMsg mkDirectionsRequest (mkSelector "setSource:") retVoid [argPtr (castPtr raw_source :: Ptr ())]

-- | @- setDestination:@
setDestination :: (IsMKDirectionsRequest mkDirectionsRequest, IsMKMapItem destination) => mkDirectionsRequest -> destination -> IO ()
setDestination mkDirectionsRequest  destination =
  withObjCPtr destination $ \raw_destination ->
      sendMsg mkDirectionsRequest (mkSelector "setDestination:") retVoid [argPtr (castPtr raw_destination :: Ptr ())]

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsMKDirectionsRequest mkDirectionsRequest, IsNSURL url) => mkDirectionsRequest -> url -> IO (Id MKDirectionsRequest)
initWithContentsOfURL mkDirectionsRequest  url =
  withObjCPtr url $ \raw_url ->
      sendMsg mkDirectionsRequest (mkSelector "initWithContentsOfURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @+ isDirectionsRequestURL:@
isDirectionsRequestURL :: IsNSURL url => url -> IO Bool
isDirectionsRequestURL url =
  do
    cls' <- getRequiredClass "MKDirectionsRequest"
    withObjCPtr url $ \raw_url ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isDirectionsRequestURL:") retCULong [argPtr (castPtr raw_url :: Ptr ())]

-- | @- source@
source :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> IO (Id MKMapItem)
source mkDirectionsRequest  =
    sendMsg mkDirectionsRequest (mkSelector "source") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- destination@
destination :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> IO (Id MKMapItem)
destination mkDirectionsRequest  =
    sendMsg mkDirectionsRequest (mkSelector "destination") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- transportType@
transportType :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> IO MKDirectionsTransportType
transportType mkDirectionsRequest  =
    fmap (coerce :: CULong -> MKDirectionsTransportType) $ sendMsg mkDirectionsRequest (mkSelector "transportType") retCULong []

-- | @- setTransportType:@
setTransportType :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> MKDirectionsTransportType -> IO ()
setTransportType mkDirectionsRequest  value =
    sendMsg mkDirectionsRequest (mkSelector "setTransportType:") retVoid [argCULong (coerce value)]

-- | @- requestsAlternateRoutes@
requestsAlternateRoutes :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> IO Bool
requestsAlternateRoutes mkDirectionsRequest  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkDirectionsRequest (mkSelector "requestsAlternateRoutes") retCULong []

-- | @- setRequestsAlternateRoutes:@
setRequestsAlternateRoutes :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> Bool -> IO ()
setRequestsAlternateRoutes mkDirectionsRequest  value =
    sendMsg mkDirectionsRequest (mkSelector "setRequestsAlternateRoutes:") retVoid [argCULong (if value then 1 else 0)]

-- | @- departureDate@
departureDate :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> IO RawId
departureDate mkDirectionsRequest  =
    fmap (RawId . castPtr) $ sendMsg mkDirectionsRequest (mkSelector "departureDate") (retPtr retVoid) []

-- | @- setDepartureDate:@
setDepartureDate :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> RawId -> IO ()
setDepartureDate mkDirectionsRequest  value =
    sendMsg mkDirectionsRequest (mkSelector "setDepartureDate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- arrivalDate@
arrivalDate :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> IO RawId
arrivalDate mkDirectionsRequest  =
    fmap (RawId . castPtr) $ sendMsg mkDirectionsRequest (mkSelector "arrivalDate") (retPtr retVoid) []

-- | @- setArrivalDate:@
setArrivalDate :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> RawId -> IO ()
setArrivalDate mkDirectionsRequest  value =
    sendMsg mkDirectionsRequest (mkSelector "setArrivalDate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- tollPreference@
tollPreference :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> IO MKDirectionsRoutePreference
tollPreference mkDirectionsRequest  =
    fmap (coerce :: CLong -> MKDirectionsRoutePreference) $ sendMsg mkDirectionsRequest (mkSelector "tollPreference") retCLong []

-- | @- setTollPreference:@
setTollPreference :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> MKDirectionsRoutePreference -> IO ()
setTollPreference mkDirectionsRequest  value =
    sendMsg mkDirectionsRequest (mkSelector "setTollPreference:") retVoid [argCLong (coerce value)]

-- | @- highwayPreference@
highwayPreference :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> IO MKDirectionsRoutePreference
highwayPreference mkDirectionsRequest  =
    fmap (coerce :: CLong -> MKDirectionsRoutePreference) $ sendMsg mkDirectionsRequest (mkSelector "highwayPreference") retCLong []

-- | @- setHighwayPreference:@
setHighwayPreference :: IsMKDirectionsRequest mkDirectionsRequest => mkDirectionsRequest -> MKDirectionsRoutePreference -> IO ()
setHighwayPreference mkDirectionsRequest  value =
    sendMsg mkDirectionsRequest (mkSelector "setHighwayPreference:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setSource:@
setSourceSelector :: Selector
setSourceSelector = mkSelector "setSource:"

-- | @Selector@ for @setDestination:@
setDestinationSelector :: Selector
setDestinationSelector = mkSelector "setDestination:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @isDirectionsRequestURL:@
isDirectionsRequestURLSelector :: Selector
isDirectionsRequestURLSelector = mkSelector "isDirectionsRequestURL:"

-- | @Selector@ for @source@
sourceSelector :: Selector
sourceSelector = mkSelector "source"

-- | @Selector@ for @destination@
destinationSelector :: Selector
destinationSelector = mkSelector "destination"

-- | @Selector@ for @transportType@
transportTypeSelector :: Selector
transportTypeSelector = mkSelector "transportType"

-- | @Selector@ for @setTransportType:@
setTransportTypeSelector :: Selector
setTransportTypeSelector = mkSelector "setTransportType:"

-- | @Selector@ for @requestsAlternateRoutes@
requestsAlternateRoutesSelector :: Selector
requestsAlternateRoutesSelector = mkSelector "requestsAlternateRoutes"

-- | @Selector@ for @setRequestsAlternateRoutes:@
setRequestsAlternateRoutesSelector :: Selector
setRequestsAlternateRoutesSelector = mkSelector "setRequestsAlternateRoutes:"

-- | @Selector@ for @departureDate@
departureDateSelector :: Selector
departureDateSelector = mkSelector "departureDate"

-- | @Selector@ for @setDepartureDate:@
setDepartureDateSelector :: Selector
setDepartureDateSelector = mkSelector "setDepartureDate:"

-- | @Selector@ for @arrivalDate@
arrivalDateSelector :: Selector
arrivalDateSelector = mkSelector "arrivalDate"

-- | @Selector@ for @setArrivalDate:@
setArrivalDateSelector :: Selector
setArrivalDateSelector = mkSelector "setArrivalDate:"

-- | @Selector@ for @tollPreference@
tollPreferenceSelector :: Selector
tollPreferenceSelector = mkSelector "tollPreference"

-- | @Selector@ for @setTollPreference:@
setTollPreferenceSelector :: Selector
setTollPreferenceSelector = mkSelector "setTollPreference:"

-- | @Selector@ for @highwayPreference@
highwayPreferenceSelector :: Selector
highwayPreferenceSelector = mkSelector "highwayPreference"

-- | @Selector@ for @setHighwayPreference:@
setHighwayPreferenceSelector :: Selector
setHighwayPreferenceSelector = mkSelector "setHighwayPreference:"

