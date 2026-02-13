{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMapItem@.
module ObjC.MapKit.MKMapItem
  ( MKMapItem
  , IsMKMapItem(..)
  , mapItemForCurrentLocation
  , initWithPlacemark
  , initWithLocation_address
  , openInMapsWithLaunchOptions
  , openMapsWithItems_launchOptions
  , openInMapsWithLaunchOptions_completionHandler
  , openMapsWithItems_launchOptions_completionHandler
  , identifier
  , alternateIdentifiers
  , placemark
  , isCurrentLocation
  , address
  , addressRepresentations
  , name
  , setName
  , phoneNumber
  , setPhoneNumber
  , url
  , setUrl
  , timeZone
  , setTimeZone
  , pointOfInterestCategory
  , setPointOfInterestCategory
  , addressRepresentationsSelector
  , addressSelector
  , alternateIdentifiersSelector
  , identifierSelector
  , initWithLocation_addressSelector
  , initWithPlacemarkSelector
  , isCurrentLocationSelector
  , mapItemForCurrentLocationSelector
  , nameSelector
  , openInMapsWithLaunchOptionsSelector
  , openInMapsWithLaunchOptions_completionHandlerSelector
  , openMapsWithItems_launchOptionsSelector
  , openMapsWithItems_launchOptions_completionHandlerSelector
  , phoneNumberSelector
  , placemarkSelector
  , pointOfInterestCategorySelector
  , setNameSelector
  , setPhoneNumberSelector
  , setPointOfInterestCategorySelector
  , setTimeZoneSelector
  , setUrlSelector
  , timeZoneSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ mapItemForCurrentLocation@
mapItemForCurrentLocation :: IO (Id MKMapItem)
mapItemForCurrentLocation  =
  do
    cls' <- getRequiredClass "MKMapItem"
    sendClassMessage cls' mapItemForCurrentLocationSelector

-- | @- initWithPlacemark:@
initWithPlacemark :: (IsMKMapItem mkMapItem, IsMKPlacemark placemark) => mkMapItem -> placemark -> IO (Id MKMapItem)
initWithPlacemark mkMapItem placemark =
  sendOwnedMessage mkMapItem initWithPlacemarkSelector (toMKPlacemark placemark)

-- | @- initWithLocation:address:@
initWithLocation_address :: (IsMKMapItem mkMapItem, IsMKAddress address) => mkMapItem -> RawId -> address -> IO (Id MKMapItem)
initWithLocation_address mkMapItem location address =
  sendOwnedMessage mkMapItem initWithLocation_addressSelector location (toMKAddress address)

-- | @- openInMapsWithLaunchOptions:@
openInMapsWithLaunchOptions :: (IsMKMapItem mkMapItem, IsNSDictionary launchOptions) => mkMapItem -> launchOptions -> IO Bool
openInMapsWithLaunchOptions mkMapItem launchOptions =
  sendMessage mkMapItem openInMapsWithLaunchOptionsSelector (toNSDictionary launchOptions)

-- | @+ openMapsWithItems:launchOptions:@
openMapsWithItems_launchOptions :: (IsNSArray mapItems, IsNSDictionary launchOptions) => mapItems -> launchOptions -> IO Bool
openMapsWithItems_launchOptions mapItems launchOptions =
  do
    cls' <- getRequiredClass "MKMapItem"
    sendClassMessage cls' openMapsWithItems_launchOptionsSelector (toNSArray mapItems) (toNSDictionary launchOptions)

-- | @- openInMapsWithLaunchOptions:completionHandler:@
openInMapsWithLaunchOptions_completionHandler :: (IsMKMapItem mkMapItem, IsNSDictionary launchOptions) => mkMapItem -> launchOptions -> Ptr () -> IO ()
openInMapsWithLaunchOptions_completionHandler mkMapItem launchOptions completion =
  sendMessage mkMapItem openInMapsWithLaunchOptions_completionHandlerSelector (toNSDictionary launchOptions) completion

-- | @+ openMapsWithItems:launchOptions:completionHandler:@
openMapsWithItems_launchOptions_completionHandler :: (IsNSArray mapItems, IsNSDictionary launchOptions) => mapItems -> launchOptions -> Ptr () -> IO ()
openMapsWithItems_launchOptions_completionHandler mapItems launchOptions completion =
  do
    cls' <- getRequiredClass "MKMapItem"
    sendClassMessage cls' openMapsWithItems_launchOptions_completionHandlerSelector (toNSArray mapItems) (toNSDictionary launchOptions) completion

-- | @- identifier@
identifier :: IsMKMapItem mkMapItem => mkMapItem -> IO (Id MKMapItemIdentifier)
identifier mkMapItem =
  sendMessage mkMapItem identifierSelector

-- | @- alternateIdentifiers@
alternateIdentifiers :: IsMKMapItem mkMapItem => mkMapItem -> IO (Id NSSet)
alternateIdentifiers mkMapItem =
  sendMessage mkMapItem alternateIdentifiersSelector

-- | @- placemark@
placemark :: IsMKMapItem mkMapItem => mkMapItem -> IO (Id MKPlacemark)
placemark mkMapItem =
  sendMessage mkMapItem placemarkSelector

-- | @- isCurrentLocation@
isCurrentLocation :: IsMKMapItem mkMapItem => mkMapItem -> IO Bool
isCurrentLocation mkMapItem =
  sendMessage mkMapItem isCurrentLocationSelector

-- | @- address@
address :: IsMKMapItem mkMapItem => mkMapItem -> IO (Id MKAddress)
address mkMapItem =
  sendMessage mkMapItem addressSelector

-- | @- addressRepresentations@
addressRepresentations :: IsMKMapItem mkMapItem => mkMapItem -> IO (Id MKAddressRepresentations)
addressRepresentations mkMapItem =
  sendMessage mkMapItem addressRepresentationsSelector

-- | @- name@
name :: IsMKMapItem mkMapItem => mkMapItem -> IO (Id NSString)
name mkMapItem =
  sendMessage mkMapItem nameSelector

-- | @- setName:@
setName :: (IsMKMapItem mkMapItem, IsNSString value) => mkMapItem -> value -> IO ()
setName mkMapItem value =
  sendMessage mkMapItem setNameSelector (toNSString value)

-- | @- phoneNumber@
phoneNumber :: IsMKMapItem mkMapItem => mkMapItem -> IO (Id NSString)
phoneNumber mkMapItem =
  sendMessage mkMapItem phoneNumberSelector

-- | @- setPhoneNumber:@
setPhoneNumber :: (IsMKMapItem mkMapItem, IsNSString value) => mkMapItem -> value -> IO ()
setPhoneNumber mkMapItem value =
  sendMessage mkMapItem setPhoneNumberSelector (toNSString value)

-- | @- url@
url :: IsMKMapItem mkMapItem => mkMapItem -> IO (Id NSURL)
url mkMapItem =
  sendMessage mkMapItem urlSelector

-- | @- setUrl:@
setUrl :: (IsMKMapItem mkMapItem, IsNSURL value) => mkMapItem -> value -> IO ()
setUrl mkMapItem value =
  sendMessage mkMapItem setUrlSelector (toNSURL value)

-- | @- timeZone@
timeZone :: IsMKMapItem mkMapItem => mkMapItem -> IO RawId
timeZone mkMapItem =
  sendMessage mkMapItem timeZoneSelector

-- | @- setTimeZone:@
setTimeZone :: IsMKMapItem mkMapItem => mkMapItem -> RawId -> IO ()
setTimeZone mkMapItem value =
  sendMessage mkMapItem setTimeZoneSelector value

-- | @- pointOfInterestCategory@
pointOfInterestCategory :: IsMKMapItem mkMapItem => mkMapItem -> IO (Id NSString)
pointOfInterestCategory mkMapItem =
  sendMessage mkMapItem pointOfInterestCategorySelector

-- | @- setPointOfInterestCategory:@
setPointOfInterestCategory :: (IsMKMapItem mkMapItem, IsNSString value) => mkMapItem -> value -> IO ()
setPointOfInterestCategory mkMapItem value =
  sendMessage mkMapItem setPointOfInterestCategorySelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mapItemForCurrentLocation@
mapItemForCurrentLocationSelector :: Selector '[] (Id MKMapItem)
mapItemForCurrentLocationSelector = mkSelector "mapItemForCurrentLocation"

-- | @Selector@ for @initWithPlacemark:@
initWithPlacemarkSelector :: Selector '[Id MKPlacemark] (Id MKMapItem)
initWithPlacemarkSelector = mkSelector "initWithPlacemark:"

-- | @Selector@ for @initWithLocation:address:@
initWithLocation_addressSelector :: Selector '[RawId, Id MKAddress] (Id MKMapItem)
initWithLocation_addressSelector = mkSelector "initWithLocation:address:"

-- | @Selector@ for @openInMapsWithLaunchOptions:@
openInMapsWithLaunchOptionsSelector :: Selector '[Id NSDictionary] Bool
openInMapsWithLaunchOptionsSelector = mkSelector "openInMapsWithLaunchOptions:"

-- | @Selector@ for @openMapsWithItems:launchOptions:@
openMapsWithItems_launchOptionsSelector :: Selector '[Id NSArray, Id NSDictionary] Bool
openMapsWithItems_launchOptionsSelector = mkSelector "openMapsWithItems:launchOptions:"

-- | @Selector@ for @openInMapsWithLaunchOptions:completionHandler:@
openInMapsWithLaunchOptions_completionHandlerSelector :: Selector '[Id NSDictionary, Ptr ()] ()
openInMapsWithLaunchOptions_completionHandlerSelector = mkSelector "openInMapsWithLaunchOptions:completionHandler:"

-- | @Selector@ for @openMapsWithItems:launchOptions:completionHandler:@
openMapsWithItems_launchOptions_completionHandlerSelector :: Selector '[Id NSArray, Id NSDictionary, Ptr ()] ()
openMapsWithItems_launchOptions_completionHandlerSelector = mkSelector "openMapsWithItems:launchOptions:completionHandler:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id MKMapItemIdentifier)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @alternateIdentifiers@
alternateIdentifiersSelector :: Selector '[] (Id NSSet)
alternateIdentifiersSelector = mkSelector "alternateIdentifiers"

-- | @Selector@ for @placemark@
placemarkSelector :: Selector '[] (Id MKPlacemark)
placemarkSelector = mkSelector "placemark"

-- | @Selector@ for @isCurrentLocation@
isCurrentLocationSelector :: Selector '[] Bool
isCurrentLocationSelector = mkSelector "isCurrentLocation"

-- | @Selector@ for @address@
addressSelector :: Selector '[] (Id MKAddress)
addressSelector = mkSelector "address"

-- | @Selector@ for @addressRepresentations@
addressRepresentationsSelector :: Selector '[] (Id MKAddressRepresentations)
addressRepresentationsSelector = mkSelector "addressRepresentations"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @phoneNumber@
phoneNumberSelector :: Selector '[] (Id NSString)
phoneNumberSelector = mkSelector "phoneNumber"

-- | @Selector@ for @setPhoneNumber:@
setPhoneNumberSelector :: Selector '[Id NSString] ()
setPhoneNumberSelector = mkSelector "setPhoneNumber:"

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

-- | @Selector@ for @setUrl:@
setUrlSelector :: Selector '[Id NSURL] ()
setUrlSelector = mkSelector "setUrl:"

-- | @Selector@ for @timeZone@
timeZoneSelector :: Selector '[] RawId
timeZoneSelector = mkSelector "timeZone"

-- | @Selector@ for @setTimeZone:@
setTimeZoneSelector :: Selector '[RawId] ()
setTimeZoneSelector = mkSelector "setTimeZone:"

-- | @Selector@ for @pointOfInterestCategory@
pointOfInterestCategorySelector :: Selector '[] (Id NSString)
pointOfInterestCategorySelector = mkSelector "pointOfInterestCategory"

-- | @Selector@ for @setPointOfInterestCategory:@
setPointOfInterestCategorySelector :: Selector '[Id NSString] ()
setPointOfInterestCategorySelector = mkSelector "setPointOfInterestCategory:"

