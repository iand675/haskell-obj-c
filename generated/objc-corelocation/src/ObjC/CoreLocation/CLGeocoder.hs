{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLGeocoder@.
module ObjC.CoreLocation.CLGeocoder
  ( CLGeocoder
  , IsCLGeocoder(..)
  , reverseGeocodeLocation_completionHandler
  , reverseGeocodeLocation_preferredLocale_completionHandler
  , geocodeAddressDictionary_completionHandler
  , geocodeAddressString_inRegion_completionHandler
  , geocodeAddressString_inRegion_preferredLocale_completionHandler
  , geocodeAddressString_inRegionCenteredAt_inRegionRadius_preferredLocale_completionHandler
  , geocodeAddressString_completionHandler
  , cancelGeocode
  , geocodePostalAddress_completionHandler
  , geocodePostalAddress_preferredLocale_completionHandler
  , geocoding
  , reverseGeocodeLocation_completionHandlerSelector
  , reverseGeocodeLocation_preferredLocale_completionHandlerSelector
  , geocodeAddressDictionary_completionHandlerSelector
  , geocodeAddressString_inRegion_completionHandlerSelector
  , geocodeAddressString_inRegion_preferredLocale_completionHandlerSelector
  , geocodeAddressString_inRegionCenteredAt_inRegionRadius_preferredLocale_completionHandlerSelector
  , geocodeAddressString_completionHandlerSelector
  , cancelGeocodeSelector
  , geocodePostalAddress_completionHandlerSelector
  , geocodePostalAddress_preferredLocale_completionHandlerSelector
  , geocodingSelector


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
import ObjC.CoreLocation.Internal.Structs
import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- reverseGeocodeLocation:completionHandler:@
reverseGeocodeLocation_completionHandler :: (IsCLGeocoder clGeocoder, IsCLLocation location) => clGeocoder -> location -> Ptr () -> IO ()
reverseGeocodeLocation_completionHandler clGeocoder  location completionHandler =
withObjCPtr location $ \raw_location ->
    sendMsg clGeocoder (mkSelector "reverseGeocodeLocation:completionHandler:") retVoid [argPtr (castPtr raw_location :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- reverseGeocodeLocation:preferredLocale:completionHandler:@
reverseGeocodeLocation_preferredLocale_completionHandler :: (IsCLGeocoder clGeocoder, IsCLLocation location, IsNSLocale locale) => clGeocoder -> location -> locale -> Ptr () -> IO ()
reverseGeocodeLocation_preferredLocale_completionHandler clGeocoder  location locale completionHandler =
withObjCPtr location $ \raw_location ->
  withObjCPtr locale $ \raw_locale ->
      sendMsg clGeocoder (mkSelector "reverseGeocodeLocation:preferredLocale:completionHandler:") retVoid [argPtr (castPtr raw_location :: Ptr ()), argPtr (castPtr raw_locale :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- geocodeAddressDictionary:completionHandler:@
geocodeAddressDictionary_completionHandler :: (IsCLGeocoder clGeocoder, IsNSDictionary addressDictionary) => clGeocoder -> addressDictionary -> Ptr () -> IO ()
geocodeAddressDictionary_completionHandler clGeocoder  addressDictionary completionHandler =
withObjCPtr addressDictionary $ \raw_addressDictionary ->
    sendMsg clGeocoder (mkSelector "geocodeAddressDictionary:completionHandler:") retVoid [argPtr (castPtr raw_addressDictionary :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- geocodeAddressString:inRegion:completionHandler:@
geocodeAddressString_inRegion_completionHandler :: (IsCLGeocoder clGeocoder, IsNSString addressString, IsCLRegion region) => clGeocoder -> addressString -> region -> Ptr () -> IO ()
geocodeAddressString_inRegion_completionHandler clGeocoder  addressString region completionHandler =
withObjCPtr addressString $ \raw_addressString ->
  withObjCPtr region $ \raw_region ->
      sendMsg clGeocoder (mkSelector "geocodeAddressString:inRegion:completionHandler:") retVoid [argPtr (castPtr raw_addressString :: Ptr ()), argPtr (castPtr raw_region :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- geocodeAddressString:inRegion:preferredLocale:completionHandler:@
geocodeAddressString_inRegion_preferredLocale_completionHandler :: (IsCLGeocoder clGeocoder, IsNSString addressString, IsCLRegion region, IsNSLocale locale) => clGeocoder -> addressString -> region -> locale -> Ptr () -> IO ()
geocodeAddressString_inRegion_preferredLocale_completionHandler clGeocoder  addressString region locale completionHandler =
withObjCPtr addressString $ \raw_addressString ->
  withObjCPtr region $ \raw_region ->
    withObjCPtr locale $ \raw_locale ->
        sendMsg clGeocoder (mkSelector "geocodeAddressString:inRegion:preferredLocale:completionHandler:") retVoid [argPtr (castPtr raw_addressString :: Ptr ()), argPtr (castPtr raw_region :: Ptr ()), argPtr (castPtr raw_locale :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- geocodeAddressString:inRegionCenteredAt:inRegionRadius:preferredLocale:completionHandler:@
geocodeAddressString_inRegionCenteredAt_inRegionRadius_preferredLocale_completionHandler :: (IsCLGeocoder clGeocoder, IsNSString addressString, IsNSLocale locale) => clGeocoder -> addressString -> CLLocationCoordinate2D -> CDouble -> locale -> Ptr () -> IO ()
geocodeAddressString_inRegionCenteredAt_inRegionRadius_preferredLocale_completionHandler clGeocoder  addressString centroid radius locale completionHandler =
withObjCPtr addressString $ \raw_addressString ->
  withObjCPtr locale $ \raw_locale ->
      sendMsg clGeocoder (mkSelector "geocodeAddressString:inRegionCenteredAt:inRegionRadius:preferredLocale:completionHandler:") retVoid [argPtr (castPtr raw_addressString :: Ptr ()), argCLLocationCoordinate2D centroid, argCDouble (fromIntegral radius), argPtr (castPtr raw_locale :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- geocodeAddressString:completionHandler:@
geocodeAddressString_completionHandler :: (IsCLGeocoder clGeocoder, IsNSString addressString) => clGeocoder -> addressString -> Ptr () -> IO ()
geocodeAddressString_completionHandler clGeocoder  addressString completionHandler =
withObjCPtr addressString $ \raw_addressString ->
    sendMsg clGeocoder (mkSelector "geocodeAddressString:completionHandler:") retVoid [argPtr (castPtr raw_addressString :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- cancelGeocode@
cancelGeocode :: IsCLGeocoder clGeocoder => clGeocoder -> IO ()
cancelGeocode clGeocoder  =
  sendMsg clGeocoder (mkSelector "cancelGeocode") retVoid []

-- | @- geocodePostalAddress:completionHandler:@
geocodePostalAddress_completionHandler :: (IsCLGeocoder clGeocoder, IsCNPostalAddress postalAddress) => clGeocoder -> postalAddress -> Ptr () -> IO ()
geocodePostalAddress_completionHandler clGeocoder  postalAddress completionHandler =
withObjCPtr postalAddress $ \raw_postalAddress ->
    sendMsg clGeocoder (mkSelector "geocodePostalAddress:completionHandler:") retVoid [argPtr (castPtr raw_postalAddress :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- geocodePostalAddress:preferredLocale:completionHandler:@
geocodePostalAddress_preferredLocale_completionHandler :: (IsCLGeocoder clGeocoder, IsCNPostalAddress postalAddress, IsNSLocale locale) => clGeocoder -> postalAddress -> locale -> Ptr () -> IO ()
geocodePostalAddress_preferredLocale_completionHandler clGeocoder  postalAddress locale completionHandler =
withObjCPtr postalAddress $ \raw_postalAddress ->
  withObjCPtr locale $ \raw_locale ->
      sendMsg clGeocoder (mkSelector "geocodePostalAddress:preferredLocale:completionHandler:") retVoid [argPtr (castPtr raw_postalAddress :: Ptr ()), argPtr (castPtr raw_locale :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- geocoding@
geocoding :: IsCLGeocoder clGeocoder => clGeocoder -> IO Bool
geocoding clGeocoder  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clGeocoder (mkSelector "geocoding") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reverseGeocodeLocation:completionHandler:@
reverseGeocodeLocation_completionHandlerSelector :: Selector
reverseGeocodeLocation_completionHandlerSelector = mkSelector "reverseGeocodeLocation:completionHandler:"

-- | @Selector@ for @reverseGeocodeLocation:preferredLocale:completionHandler:@
reverseGeocodeLocation_preferredLocale_completionHandlerSelector :: Selector
reverseGeocodeLocation_preferredLocale_completionHandlerSelector = mkSelector "reverseGeocodeLocation:preferredLocale:completionHandler:"

-- | @Selector@ for @geocodeAddressDictionary:completionHandler:@
geocodeAddressDictionary_completionHandlerSelector :: Selector
geocodeAddressDictionary_completionHandlerSelector = mkSelector "geocodeAddressDictionary:completionHandler:"

-- | @Selector@ for @geocodeAddressString:inRegion:completionHandler:@
geocodeAddressString_inRegion_completionHandlerSelector :: Selector
geocodeAddressString_inRegion_completionHandlerSelector = mkSelector "geocodeAddressString:inRegion:completionHandler:"

-- | @Selector@ for @geocodeAddressString:inRegion:preferredLocale:completionHandler:@
geocodeAddressString_inRegion_preferredLocale_completionHandlerSelector :: Selector
geocodeAddressString_inRegion_preferredLocale_completionHandlerSelector = mkSelector "geocodeAddressString:inRegion:preferredLocale:completionHandler:"

-- | @Selector@ for @geocodeAddressString:inRegionCenteredAt:inRegionRadius:preferredLocale:completionHandler:@
geocodeAddressString_inRegionCenteredAt_inRegionRadius_preferredLocale_completionHandlerSelector :: Selector
geocodeAddressString_inRegionCenteredAt_inRegionRadius_preferredLocale_completionHandlerSelector = mkSelector "geocodeAddressString:inRegionCenteredAt:inRegionRadius:preferredLocale:completionHandler:"

-- | @Selector@ for @geocodeAddressString:completionHandler:@
geocodeAddressString_completionHandlerSelector :: Selector
geocodeAddressString_completionHandlerSelector = mkSelector "geocodeAddressString:completionHandler:"

-- | @Selector@ for @cancelGeocode@
cancelGeocodeSelector :: Selector
cancelGeocodeSelector = mkSelector "cancelGeocode"

-- | @Selector@ for @geocodePostalAddress:completionHandler:@
geocodePostalAddress_completionHandlerSelector :: Selector
geocodePostalAddress_completionHandlerSelector = mkSelector "geocodePostalAddress:completionHandler:"

-- | @Selector@ for @geocodePostalAddress:preferredLocale:completionHandler:@
geocodePostalAddress_preferredLocale_completionHandlerSelector :: Selector
geocodePostalAddress_preferredLocale_completionHandlerSelector = mkSelector "geocodePostalAddress:preferredLocale:completionHandler:"

-- | @Selector@ for @geocoding@
geocodingSelector :: Selector
geocodingSelector = mkSelector "geocoding"

