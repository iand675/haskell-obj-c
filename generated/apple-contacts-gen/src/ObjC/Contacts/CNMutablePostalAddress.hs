{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A mutable value object representing a postal address.
--
-- CNMutablePostalAddress is not thread safe.
--
-- Note: To remove properties when saving a mutable postal address, set string properties to empty values.
--
-- Generated bindings for @CNMutablePostalAddress@.
module ObjC.Contacts.CNMutablePostalAddress
  ( CNMutablePostalAddress
  , IsCNMutablePostalAddress(..)
  , street
  , setStreet
  , subLocality
  , setSubLocality
  , city
  , setCity
  , subAdministrativeArea
  , setSubAdministrativeArea
  , state
  , setState
  , postalCode
  , setPostalCode
  , country
  , setCountry
  , isoCountryCode
  , setISOCountryCode
  , citySelector
  , countrySelector
  , isoCountryCodeSelector
  , postalCodeSelector
  , setCitySelector
  , setCountrySelector
  , setISOCountryCodeSelector
  , setPostalCodeSelector
  , setStateSelector
  , setStreetSelector
  , setSubAdministrativeAreaSelector
  , setSubLocalitySelector
  , stateSelector
  , streetSelector
  , subAdministrativeAreaSelector
  , subLocalitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | multi-street address is delimited with carriage returns “”
--
-- ObjC selector: @- street@
street :: IsCNMutablePostalAddress cnMutablePostalAddress => cnMutablePostalAddress -> IO (Id NSString)
street cnMutablePostalAddress =
  sendMessage cnMutablePostalAddress streetSelector

-- | multi-street address is delimited with carriage returns “”
--
-- ObjC selector: @- setStreet:@
setStreet :: (IsCNMutablePostalAddress cnMutablePostalAddress, IsNSString value) => cnMutablePostalAddress -> value -> IO ()
setStreet cnMutablePostalAddress value =
  sendMessage cnMutablePostalAddress setStreetSelector (toNSString value)

-- | @- subLocality@
subLocality :: IsCNMutablePostalAddress cnMutablePostalAddress => cnMutablePostalAddress -> IO RawId
subLocality cnMutablePostalAddress =
  sendMessage cnMutablePostalAddress subLocalitySelector

-- | @- setSubLocality:@
setSubLocality :: IsCNMutablePostalAddress cnMutablePostalAddress => cnMutablePostalAddress -> RawId -> IO ()
setSubLocality cnMutablePostalAddress value =
  sendMessage cnMutablePostalAddress setSubLocalitySelector value

-- | @- city@
city :: IsCNMutablePostalAddress cnMutablePostalAddress => cnMutablePostalAddress -> IO (Id NSString)
city cnMutablePostalAddress =
  sendMessage cnMutablePostalAddress citySelector

-- | @- setCity:@
setCity :: (IsCNMutablePostalAddress cnMutablePostalAddress, IsNSString value) => cnMutablePostalAddress -> value -> IO ()
setCity cnMutablePostalAddress value =
  sendMessage cnMutablePostalAddress setCitySelector (toNSString value)

-- | @- subAdministrativeArea@
subAdministrativeArea :: IsCNMutablePostalAddress cnMutablePostalAddress => cnMutablePostalAddress -> IO RawId
subAdministrativeArea cnMutablePostalAddress =
  sendMessage cnMutablePostalAddress subAdministrativeAreaSelector

-- | @- setSubAdministrativeArea:@
setSubAdministrativeArea :: IsCNMutablePostalAddress cnMutablePostalAddress => cnMutablePostalAddress -> RawId -> IO ()
setSubAdministrativeArea cnMutablePostalAddress value =
  sendMessage cnMutablePostalAddress setSubAdministrativeAreaSelector value

-- | @- state@
state :: IsCNMutablePostalAddress cnMutablePostalAddress => cnMutablePostalAddress -> IO (Id NSString)
state cnMutablePostalAddress =
  sendMessage cnMutablePostalAddress stateSelector

-- | @- setState:@
setState :: (IsCNMutablePostalAddress cnMutablePostalAddress, IsNSString value) => cnMutablePostalAddress -> value -> IO ()
setState cnMutablePostalAddress value =
  sendMessage cnMutablePostalAddress setStateSelector (toNSString value)

-- | @- postalCode@
postalCode :: IsCNMutablePostalAddress cnMutablePostalAddress => cnMutablePostalAddress -> IO (Id NSString)
postalCode cnMutablePostalAddress =
  sendMessage cnMutablePostalAddress postalCodeSelector

-- | @- setPostalCode:@
setPostalCode :: (IsCNMutablePostalAddress cnMutablePostalAddress, IsNSString value) => cnMutablePostalAddress -> value -> IO ()
setPostalCode cnMutablePostalAddress value =
  sendMessage cnMutablePostalAddress setPostalCodeSelector (toNSString value)

-- | @- country@
country :: IsCNMutablePostalAddress cnMutablePostalAddress => cnMutablePostalAddress -> IO (Id NSString)
country cnMutablePostalAddress =
  sendMessage cnMutablePostalAddress countrySelector

-- | @- setCountry:@
setCountry :: (IsCNMutablePostalAddress cnMutablePostalAddress, IsNSString value) => cnMutablePostalAddress -> value -> IO ()
setCountry cnMutablePostalAddress value =
  sendMessage cnMutablePostalAddress setCountrySelector (toNSString value)

-- | @- ISOCountryCode@
isoCountryCode :: IsCNMutablePostalAddress cnMutablePostalAddress => cnMutablePostalAddress -> IO (Id NSString)
isoCountryCode cnMutablePostalAddress =
  sendMessage cnMutablePostalAddress isoCountryCodeSelector

-- | @- setISOCountryCode:@
setISOCountryCode :: (IsCNMutablePostalAddress cnMutablePostalAddress, IsNSString value) => cnMutablePostalAddress -> value -> IO ()
setISOCountryCode cnMutablePostalAddress value =
  sendMessage cnMutablePostalAddress setISOCountryCodeSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @street@
streetSelector :: Selector '[] (Id NSString)
streetSelector = mkSelector "street"

-- | @Selector@ for @setStreet:@
setStreetSelector :: Selector '[Id NSString] ()
setStreetSelector = mkSelector "setStreet:"

-- | @Selector@ for @subLocality@
subLocalitySelector :: Selector '[] RawId
subLocalitySelector = mkSelector "subLocality"

-- | @Selector@ for @setSubLocality:@
setSubLocalitySelector :: Selector '[RawId] ()
setSubLocalitySelector = mkSelector "setSubLocality:"

-- | @Selector@ for @city@
citySelector :: Selector '[] (Id NSString)
citySelector = mkSelector "city"

-- | @Selector@ for @setCity:@
setCitySelector :: Selector '[Id NSString] ()
setCitySelector = mkSelector "setCity:"

-- | @Selector@ for @subAdministrativeArea@
subAdministrativeAreaSelector :: Selector '[] RawId
subAdministrativeAreaSelector = mkSelector "subAdministrativeArea"

-- | @Selector@ for @setSubAdministrativeArea:@
setSubAdministrativeAreaSelector :: Selector '[RawId] ()
setSubAdministrativeAreaSelector = mkSelector "setSubAdministrativeArea:"

-- | @Selector@ for @state@
stateSelector :: Selector '[] (Id NSString)
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector '[Id NSString] ()
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @postalCode@
postalCodeSelector :: Selector '[] (Id NSString)
postalCodeSelector = mkSelector "postalCode"

-- | @Selector@ for @setPostalCode:@
setPostalCodeSelector :: Selector '[Id NSString] ()
setPostalCodeSelector = mkSelector "setPostalCode:"

-- | @Selector@ for @country@
countrySelector :: Selector '[] (Id NSString)
countrySelector = mkSelector "country"

-- | @Selector@ for @setCountry:@
setCountrySelector :: Selector '[Id NSString] ()
setCountrySelector = mkSelector "setCountry:"

-- | @Selector@ for @ISOCountryCode@
isoCountryCodeSelector :: Selector '[] (Id NSString)
isoCountryCodeSelector = mkSelector "ISOCountryCode"

-- | @Selector@ for @setISOCountryCode:@
setISOCountryCodeSelector :: Selector '[Id NSString] ()
setISOCountryCodeSelector = mkSelector "setISOCountryCode:"

