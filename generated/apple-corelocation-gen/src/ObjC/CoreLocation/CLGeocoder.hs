{-# LANGUAGE DataKinds #-}
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
  , geocodeAddressString_completionHandler
  , cancelGeocode
  , geocodePostalAddress_completionHandler
  , geocodePostalAddress_preferredLocale_completionHandler
  , geocoding
  , cancelGeocodeSelector
  , geocodeAddressDictionary_completionHandlerSelector
  , geocodeAddressString_completionHandlerSelector
  , geocodeAddressString_inRegion_completionHandlerSelector
  , geocodeAddressString_inRegion_preferredLocale_completionHandlerSelector
  , geocodePostalAddress_completionHandlerSelector
  , geocodePostalAddress_preferredLocale_completionHandlerSelector
  , geocodingSelector
  , reverseGeocodeLocation_completionHandlerSelector
  , reverseGeocodeLocation_preferredLocale_completionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreLocation.Internal.Classes
import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- reverseGeocodeLocation:completionHandler:@
reverseGeocodeLocation_completionHandler :: IsCLGeocoder clGeocoder => clGeocoder -> RawId -> Ptr () -> IO ()
reverseGeocodeLocation_completionHandler clGeocoder location completionHandler =
  sendMessage clGeocoder reverseGeocodeLocation_completionHandlerSelector location completionHandler

-- | @- reverseGeocodeLocation:preferredLocale:completionHandler:@
reverseGeocodeLocation_preferredLocale_completionHandler :: (IsCLGeocoder clGeocoder, IsNSLocale locale) => clGeocoder -> RawId -> locale -> Ptr () -> IO ()
reverseGeocodeLocation_preferredLocale_completionHandler clGeocoder location locale completionHandler =
  sendMessage clGeocoder reverseGeocodeLocation_preferredLocale_completionHandlerSelector location (toNSLocale locale) completionHandler

-- | @- geocodeAddressDictionary:completionHandler:@
geocodeAddressDictionary_completionHandler :: (IsCLGeocoder clGeocoder, IsNSDictionary addressDictionary) => clGeocoder -> addressDictionary -> Ptr () -> IO ()
geocodeAddressDictionary_completionHandler clGeocoder addressDictionary completionHandler =
  sendMessage clGeocoder geocodeAddressDictionary_completionHandlerSelector (toNSDictionary addressDictionary) completionHandler

-- | @- geocodeAddressString:inRegion:completionHandler:@
geocodeAddressString_inRegion_completionHandler :: (IsCLGeocoder clGeocoder, IsNSString addressString, IsCLRegion region) => clGeocoder -> addressString -> region -> Ptr () -> IO ()
geocodeAddressString_inRegion_completionHandler clGeocoder addressString region completionHandler =
  sendMessage clGeocoder geocodeAddressString_inRegion_completionHandlerSelector (toNSString addressString) (toCLRegion region) completionHandler

-- | @- geocodeAddressString:inRegion:preferredLocale:completionHandler:@
geocodeAddressString_inRegion_preferredLocale_completionHandler :: (IsCLGeocoder clGeocoder, IsNSString addressString, IsCLRegion region, IsNSLocale locale) => clGeocoder -> addressString -> region -> locale -> Ptr () -> IO ()
geocodeAddressString_inRegion_preferredLocale_completionHandler clGeocoder addressString region locale completionHandler =
  sendMessage clGeocoder geocodeAddressString_inRegion_preferredLocale_completionHandlerSelector (toNSString addressString) (toCLRegion region) (toNSLocale locale) completionHandler

-- | @- geocodeAddressString:completionHandler:@
geocodeAddressString_completionHandler :: (IsCLGeocoder clGeocoder, IsNSString addressString) => clGeocoder -> addressString -> Ptr () -> IO ()
geocodeAddressString_completionHandler clGeocoder addressString completionHandler =
  sendMessage clGeocoder geocodeAddressString_completionHandlerSelector (toNSString addressString) completionHandler

-- | @- cancelGeocode@
cancelGeocode :: IsCLGeocoder clGeocoder => clGeocoder -> IO ()
cancelGeocode clGeocoder =
  sendMessage clGeocoder cancelGeocodeSelector

-- | @- geocodePostalAddress:completionHandler:@
geocodePostalAddress_completionHandler :: (IsCLGeocoder clGeocoder, IsCNPostalAddress postalAddress) => clGeocoder -> postalAddress -> Ptr () -> IO ()
geocodePostalAddress_completionHandler clGeocoder postalAddress completionHandler =
  sendMessage clGeocoder geocodePostalAddress_completionHandlerSelector (toCNPostalAddress postalAddress) completionHandler

-- | @- geocodePostalAddress:preferredLocale:completionHandler:@
geocodePostalAddress_preferredLocale_completionHandler :: (IsCLGeocoder clGeocoder, IsCNPostalAddress postalAddress, IsNSLocale locale) => clGeocoder -> postalAddress -> locale -> Ptr () -> IO ()
geocodePostalAddress_preferredLocale_completionHandler clGeocoder postalAddress locale completionHandler =
  sendMessage clGeocoder geocodePostalAddress_preferredLocale_completionHandlerSelector (toCNPostalAddress postalAddress) (toNSLocale locale) completionHandler

-- | @- geocoding@
geocoding :: IsCLGeocoder clGeocoder => clGeocoder -> IO Bool
geocoding clGeocoder =
  sendMessage clGeocoder geocodingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reverseGeocodeLocation:completionHandler:@
reverseGeocodeLocation_completionHandlerSelector :: Selector '[RawId, Ptr ()] ()
reverseGeocodeLocation_completionHandlerSelector = mkSelector "reverseGeocodeLocation:completionHandler:"

-- | @Selector@ for @reverseGeocodeLocation:preferredLocale:completionHandler:@
reverseGeocodeLocation_preferredLocale_completionHandlerSelector :: Selector '[RawId, Id NSLocale, Ptr ()] ()
reverseGeocodeLocation_preferredLocale_completionHandlerSelector = mkSelector "reverseGeocodeLocation:preferredLocale:completionHandler:"

-- | @Selector@ for @geocodeAddressDictionary:completionHandler:@
geocodeAddressDictionary_completionHandlerSelector :: Selector '[Id NSDictionary, Ptr ()] ()
geocodeAddressDictionary_completionHandlerSelector = mkSelector "geocodeAddressDictionary:completionHandler:"

-- | @Selector@ for @geocodeAddressString:inRegion:completionHandler:@
geocodeAddressString_inRegion_completionHandlerSelector :: Selector '[Id NSString, Id CLRegion, Ptr ()] ()
geocodeAddressString_inRegion_completionHandlerSelector = mkSelector "geocodeAddressString:inRegion:completionHandler:"

-- | @Selector@ for @geocodeAddressString:inRegion:preferredLocale:completionHandler:@
geocodeAddressString_inRegion_preferredLocale_completionHandlerSelector :: Selector '[Id NSString, Id CLRegion, Id NSLocale, Ptr ()] ()
geocodeAddressString_inRegion_preferredLocale_completionHandlerSelector = mkSelector "geocodeAddressString:inRegion:preferredLocale:completionHandler:"

-- | @Selector@ for @geocodeAddressString:completionHandler:@
geocodeAddressString_completionHandlerSelector :: Selector '[Id NSString, Ptr ()] ()
geocodeAddressString_completionHandlerSelector = mkSelector "geocodeAddressString:completionHandler:"

-- | @Selector@ for @cancelGeocode@
cancelGeocodeSelector :: Selector '[] ()
cancelGeocodeSelector = mkSelector "cancelGeocode"

-- | @Selector@ for @geocodePostalAddress:completionHandler:@
geocodePostalAddress_completionHandlerSelector :: Selector '[Id CNPostalAddress, Ptr ()] ()
geocodePostalAddress_completionHandlerSelector = mkSelector "geocodePostalAddress:completionHandler:"

-- | @Selector@ for @geocodePostalAddress:preferredLocale:completionHandler:@
geocodePostalAddress_preferredLocale_completionHandlerSelector :: Selector '[Id CNPostalAddress, Id NSLocale, Ptr ()] ()
geocodePostalAddress_preferredLocale_completionHandlerSelector = mkSelector "geocodePostalAddress:preferredLocale:completionHandler:"

-- | @Selector@ for @geocoding@
geocodingSelector :: Selector '[] Bool
geocodingSelector = mkSelector "geocoding"

