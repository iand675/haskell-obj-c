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
  , city
  , setCity
  , state
  , setState
  , postalCode
  , setPostalCode
  , country
  , setCountry
  , isoCountryCode
  , setISOCountryCode
  , streetSelector
  , setStreetSelector
  , citySelector
  , setCitySelector
  , stateSelector
  , setStateSelector
  , postalCodeSelector
  , setPostalCodeSelector
  , countrySelector
  , setCountrySelector
  , isoCountryCodeSelector
  , setISOCountryCodeSelector


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

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | multi-street address is delimited with carriage returns “”
--
-- ObjC selector: @- street@
street :: IsCNMutablePostalAddress cnMutablePostalAddress => cnMutablePostalAddress -> IO (Id NSString)
street cnMutablePostalAddress  =
  sendMsg cnMutablePostalAddress (mkSelector "street") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | multi-street address is delimited with carriage returns “”
--
-- ObjC selector: @- setStreet:@
setStreet :: (IsCNMutablePostalAddress cnMutablePostalAddress, IsNSString value) => cnMutablePostalAddress -> value -> IO ()
setStreet cnMutablePostalAddress  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutablePostalAddress (mkSelector "setStreet:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- city@
city :: IsCNMutablePostalAddress cnMutablePostalAddress => cnMutablePostalAddress -> IO (Id NSString)
city cnMutablePostalAddress  =
  sendMsg cnMutablePostalAddress (mkSelector "city") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCity:@
setCity :: (IsCNMutablePostalAddress cnMutablePostalAddress, IsNSString value) => cnMutablePostalAddress -> value -> IO ()
setCity cnMutablePostalAddress  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutablePostalAddress (mkSelector "setCity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- state@
state :: IsCNMutablePostalAddress cnMutablePostalAddress => cnMutablePostalAddress -> IO (Id NSString)
state cnMutablePostalAddress  =
  sendMsg cnMutablePostalAddress (mkSelector "state") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setState:@
setState :: (IsCNMutablePostalAddress cnMutablePostalAddress, IsNSString value) => cnMutablePostalAddress -> value -> IO ()
setState cnMutablePostalAddress  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutablePostalAddress (mkSelector "setState:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- postalCode@
postalCode :: IsCNMutablePostalAddress cnMutablePostalAddress => cnMutablePostalAddress -> IO (Id NSString)
postalCode cnMutablePostalAddress  =
  sendMsg cnMutablePostalAddress (mkSelector "postalCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPostalCode:@
setPostalCode :: (IsCNMutablePostalAddress cnMutablePostalAddress, IsNSString value) => cnMutablePostalAddress -> value -> IO ()
setPostalCode cnMutablePostalAddress  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutablePostalAddress (mkSelector "setPostalCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- country@
country :: IsCNMutablePostalAddress cnMutablePostalAddress => cnMutablePostalAddress -> IO (Id NSString)
country cnMutablePostalAddress  =
  sendMsg cnMutablePostalAddress (mkSelector "country") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCountry:@
setCountry :: (IsCNMutablePostalAddress cnMutablePostalAddress, IsNSString value) => cnMutablePostalAddress -> value -> IO ()
setCountry cnMutablePostalAddress  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutablePostalAddress (mkSelector "setCountry:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- ISOCountryCode@
isoCountryCode :: IsCNMutablePostalAddress cnMutablePostalAddress => cnMutablePostalAddress -> IO (Id NSString)
isoCountryCode cnMutablePostalAddress  =
  sendMsg cnMutablePostalAddress (mkSelector "ISOCountryCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setISOCountryCode:@
setISOCountryCode :: (IsCNMutablePostalAddress cnMutablePostalAddress, IsNSString value) => cnMutablePostalAddress -> value -> IO ()
setISOCountryCode cnMutablePostalAddress  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutablePostalAddress (mkSelector "setISOCountryCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @street@
streetSelector :: Selector
streetSelector = mkSelector "street"

-- | @Selector@ for @setStreet:@
setStreetSelector :: Selector
setStreetSelector = mkSelector "setStreet:"

-- | @Selector@ for @city@
citySelector :: Selector
citySelector = mkSelector "city"

-- | @Selector@ for @setCity:@
setCitySelector :: Selector
setCitySelector = mkSelector "setCity:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @postalCode@
postalCodeSelector :: Selector
postalCodeSelector = mkSelector "postalCode"

-- | @Selector@ for @setPostalCode:@
setPostalCodeSelector :: Selector
setPostalCodeSelector = mkSelector "setPostalCode:"

-- | @Selector@ for @country@
countrySelector :: Selector
countrySelector = mkSelector "country"

-- | @Selector@ for @setCountry:@
setCountrySelector :: Selector
setCountrySelector = mkSelector "setCountry:"

-- | @Selector@ for @ISOCountryCode@
isoCountryCodeSelector :: Selector
isoCountryCodeSelector = mkSelector "ISOCountryCode"

-- | @Selector@ for @setISOCountryCode:@
setISOCountryCodeSelector :: Selector
setISOCountryCodeSelector = mkSelector "setISOCountryCode:"

