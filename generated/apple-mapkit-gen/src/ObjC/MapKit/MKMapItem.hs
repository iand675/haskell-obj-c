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
  , mapItemForCurrentLocationSelector
  , initWithPlacemarkSelector
  , initWithLocation_addressSelector
  , openInMapsWithLaunchOptionsSelector
  , openMapsWithItems_launchOptionsSelector
  , openInMapsWithLaunchOptions_completionHandlerSelector
  , openMapsWithItems_launchOptions_completionHandlerSelector
  , identifierSelector
  , alternateIdentifiersSelector
  , placemarkSelector
  , isCurrentLocationSelector
  , addressSelector
  , addressRepresentationsSelector
  , nameSelector
  , setNameSelector
  , phoneNumberSelector
  , setPhoneNumberSelector
  , urlSelector
  , setUrlSelector
  , timeZoneSelector
  , setTimeZoneSelector
  , pointOfInterestCategorySelector
  , setPointOfInterestCategorySelector


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
import ObjC.Foundation.Internal.Classes

-- | @+ mapItemForCurrentLocation@
mapItemForCurrentLocation :: IO (Id MKMapItem)
mapItemForCurrentLocation  =
  do
    cls' <- getRequiredClass "MKMapItem"
    sendClassMsg cls' (mkSelector "mapItemForCurrentLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- initWithPlacemark:@
initWithPlacemark :: (IsMKMapItem mkMapItem, IsMKPlacemark placemark) => mkMapItem -> placemark -> IO (Id MKMapItem)
initWithPlacemark mkMapItem  placemark =
  withObjCPtr placemark $ \raw_placemark ->
      sendMsg mkMapItem (mkSelector "initWithPlacemark:") (retPtr retVoid) [argPtr (castPtr raw_placemark :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithLocation:address:@
initWithLocation_address :: (IsMKMapItem mkMapItem, IsMKAddress address) => mkMapItem -> RawId -> address -> IO (Id MKMapItem)
initWithLocation_address mkMapItem  location address =
  withObjCPtr address $ \raw_address ->
      sendMsg mkMapItem (mkSelector "initWithLocation:address:") (retPtr retVoid) [argPtr (castPtr (unRawId location) :: Ptr ()), argPtr (castPtr raw_address :: Ptr ())] >>= ownedObject . castPtr

-- | @- openInMapsWithLaunchOptions:@
openInMapsWithLaunchOptions :: (IsMKMapItem mkMapItem, IsNSDictionary launchOptions) => mkMapItem -> launchOptions -> IO Bool
openInMapsWithLaunchOptions mkMapItem  launchOptions =
  withObjCPtr launchOptions $ \raw_launchOptions ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkMapItem (mkSelector "openInMapsWithLaunchOptions:") retCULong [argPtr (castPtr raw_launchOptions :: Ptr ())]

-- | @+ openMapsWithItems:launchOptions:@
openMapsWithItems_launchOptions :: (IsNSArray mapItems, IsNSDictionary launchOptions) => mapItems -> launchOptions -> IO Bool
openMapsWithItems_launchOptions mapItems launchOptions =
  do
    cls' <- getRequiredClass "MKMapItem"
    withObjCPtr mapItems $ \raw_mapItems ->
      withObjCPtr launchOptions $ \raw_launchOptions ->
        fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "openMapsWithItems:launchOptions:") retCULong [argPtr (castPtr raw_mapItems :: Ptr ()), argPtr (castPtr raw_launchOptions :: Ptr ())]

-- | @- openInMapsWithLaunchOptions:completionHandler:@
openInMapsWithLaunchOptions_completionHandler :: (IsMKMapItem mkMapItem, IsNSDictionary launchOptions) => mkMapItem -> launchOptions -> Ptr () -> IO ()
openInMapsWithLaunchOptions_completionHandler mkMapItem  launchOptions completion =
  withObjCPtr launchOptions $ \raw_launchOptions ->
      sendMsg mkMapItem (mkSelector "openInMapsWithLaunchOptions:completionHandler:") retVoid [argPtr (castPtr raw_launchOptions :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @+ openMapsWithItems:launchOptions:completionHandler:@
openMapsWithItems_launchOptions_completionHandler :: (IsNSArray mapItems, IsNSDictionary launchOptions) => mapItems -> launchOptions -> Ptr () -> IO ()
openMapsWithItems_launchOptions_completionHandler mapItems launchOptions completion =
  do
    cls' <- getRequiredClass "MKMapItem"
    withObjCPtr mapItems $ \raw_mapItems ->
      withObjCPtr launchOptions $ \raw_launchOptions ->
        sendClassMsg cls' (mkSelector "openMapsWithItems:launchOptions:completionHandler:") retVoid [argPtr (castPtr raw_mapItems :: Ptr ()), argPtr (castPtr raw_launchOptions :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- identifier@
identifier :: IsMKMapItem mkMapItem => mkMapItem -> IO (Id MKMapItemIdentifier)
identifier mkMapItem  =
    sendMsg mkMapItem (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- alternateIdentifiers@
alternateIdentifiers :: IsMKMapItem mkMapItem => mkMapItem -> IO (Id NSSet)
alternateIdentifiers mkMapItem  =
    sendMsg mkMapItem (mkSelector "alternateIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- placemark@
placemark :: IsMKMapItem mkMapItem => mkMapItem -> IO (Id MKPlacemark)
placemark mkMapItem  =
    sendMsg mkMapItem (mkSelector "placemark") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- isCurrentLocation@
isCurrentLocation :: IsMKMapItem mkMapItem => mkMapItem -> IO Bool
isCurrentLocation mkMapItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkMapItem (mkSelector "isCurrentLocation") retCULong []

-- | @- address@
address :: IsMKMapItem mkMapItem => mkMapItem -> IO (Id MKAddress)
address mkMapItem  =
    sendMsg mkMapItem (mkSelector "address") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- addressRepresentations@
addressRepresentations :: IsMKMapItem mkMapItem => mkMapItem -> IO (Id MKAddressRepresentations)
addressRepresentations mkMapItem  =
    sendMsg mkMapItem (mkSelector "addressRepresentations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- name@
name :: IsMKMapItem mkMapItem => mkMapItem -> IO (Id NSString)
name mkMapItem  =
    sendMsg mkMapItem (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMKMapItem mkMapItem, IsNSString value) => mkMapItem -> value -> IO ()
setName mkMapItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mkMapItem (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- phoneNumber@
phoneNumber :: IsMKMapItem mkMapItem => mkMapItem -> IO (Id NSString)
phoneNumber mkMapItem  =
    sendMsg mkMapItem (mkSelector "phoneNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPhoneNumber:@
setPhoneNumber :: (IsMKMapItem mkMapItem, IsNSString value) => mkMapItem -> value -> IO ()
setPhoneNumber mkMapItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mkMapItem (mkSelector "setPhoneNumber:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- url@
url :: IsMKMapItem mkMapItem => mkMapItem -> IO (Id NSURL)
url mkMapItem  =
    sendMsg mkMapItem (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUrl:@
setUrl :: (IsMKMapItem mkMapItem, IsNSURL value) => mkMapItem -> value -> IO ()
setUrl mkMapItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mkMapItem (mkSelector "setUrl:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- timeZone@
timeZone :: IsMKMapItem mkMapItem => mkMapItem -> IO RawId
timeZone mkMapItem  =
    fmap (RawId . castPtr) $ sendMsg mkMapItem (mkSelector "timeZone") (retPtr retVoid) []

-- | @- setTimeZone:@
setTimeZone :: IsMKMapItem mkMapItem => mkMapItem -> RawId -> IO ()
setTimeZone mkMapItem  value =
    sendMsg mkMapItem (mkSelector "setTimeZone:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- pointOfInterestCategory@
pointOfInterestCategory :: IsMKMapItem mkMapItem => mkMapItem -> IO (Id NSString)
pointOfInterestCategory mkMapItem  =
    sendMsg mkMapItem (mkSelector "pointOfInterestCategory") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPointOfInterestCategory:@
setPointOfInterestCategory :: (IsMKMapItem mkMapItem, IsNSString value) => mkMapItem -> value -> IO ()
setPointOfInterestCategory mkMapItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mkMapItem (mkSelector "setPointOfInterestCategory:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mapItemForCurrentLocation@
mapItemForCurrentLocationSelector :: Selector
mapItemForCurrentLocationSelector = mkSelector "mapItemForCurrentLocation"

-- | @Selector@ for @initWithPlacemark:@
initWithPlacemarkSelector :: Selector
initWithPlacemarkSelector = mkSelector "initWithPlacemark:"

-- | @Selector@ for @initWithLocation:address:@
initWithLocation_addressSelector :: Selector
initWithLocation_addressSelector = mkSelector "initWithLocation:address:"

-- | @Selector@ for @openInMapsWithLaunchOptions:@
openInMapsWithLaunchOptionsSelector :: Selector
openInMapsWithLaunchOptionsSelector = mkSelector "openInMapsWithLaunchOptions:"

-- | @Selector@ for @openMapsWithItems:launchOptions:@
openMapsWithItems_launchOptionsSelector :: Selector
openMapsWithItems_launchOptionsSelector = mkSelector "openMapsWithItems:launchOptions:"

-- | @Selector@ for @openInMapsWithLaunchOptions:completionHandler:@
openInMapsWithLaunchOptions_completionHandlerSelector :: Selector
openInMapsWithLaunchOptions_completionHandlerSelector = mkSelector "openInMapsWithLaunchOptions:completionHandler:"

-- | @Selector@ for @openMapsWithItems:launchOptions:completionHandler:@
openMapsWithItems_launchOptions_completionHandlerSelector :: Selector
openMapsWithItems_launchOptions_completionHandlerSelector = mkSelector "openMapsWithItems:launchOptions:completionHandler:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @alternateIdentifiers@
alternateIdentifiersSelector :: Selector
alternateIdentifiersSelector = mkSelector "alternateIdentifiers"

-- | @Selector@ for @placemark@
placemarkSelector :: Selector
placemarkSelector = mkSelector "placemark"

-- | @Selector@ for @isCurrentLocation@
isCurrentLocationSelector :: Selector
isCurrentLocationSelector = mkSelector "isCurrentLocation"

-- | @Selector@ for @address@
addressSelector :: Selector
addressSelector = mkSelector "address"

-- | @Selector@ for @addressRepresentations@
addressRepresentationsSelector :: Selector
addressRepresentationsSelector = mkSelector "addressRepresentations"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @phoneNumber@
phoneNumberSelector :: Selector
phoneNumberSelector = mkSelector "phoneNumber"

-- | @Selector@ for @setPhoneNumber:@
setPhoneNumberSelector :: Selector
setPhoneNumberSelector = mkSelector "setPhoneNumber:"

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @setUrl:@
setUrlSelector :: Selector
setUrlSelector = mkSelector "setUrl:"

-- | @Selector@ for @timeZone@
timeZoneSelector :: Selector
timeZoneSelector = mkSelector "timeZone"

-- | @Selector@ for @setTimeZone:@
setTimeZoneSelector :: Selector
setTimeZoneSelector = mkSelector "setTimeZone:"

-- | @Selector@ for @pointOfInterestCategory@
pointOfInterestCategorySelector :: Selector
pointOfInterestCategorySelector = mkSelector "pointOfInterestCategory"

-- | @Selector@ for @setPointOfInterestCategory:@
setPointOfInterestCategorySelector :: Selector
setPointOfInterestCategorySelector = mkSelector "setPointOfInterestCategory:"

