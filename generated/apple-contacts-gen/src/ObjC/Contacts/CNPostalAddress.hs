{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An immutable value object representing a postal address.
--
-- CNPostalAddress is thread safe.
--
-- Generated bindings for @CNPostalAddress@.
module ObjC.Contacts.CNPostalAddress
  ( CNPostalAddress
  , IsCNPostalAddress(..)
  , localizedStringForKey
  , street
  , subLocality
  , city
  , subAdministrativeArea
  , state
  , postalCode
  , country
  , isoCountryCode
  , citySelector
  , countrySelector
  , isoCountryCodeSelector
  , localizedStringForKeySelector
  , postalCodeSelector
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

-- | Returns a user displayable property name.
--
-- ObjC selector: @+ localizedStringForKey:@
localizedStringForKey :: IsNSString key => key -> IO (Id NSString)
localizedStringForKey key =
  do
    cls' <- getRequiredClass "CNPostalAddress"
    sendClassMessage cls' localizedStringForKeySelector (toNSString key)

-- | multi-street address is delimited with carriage returns “”
--
-- ObjC selector: @- street@
street :: IsCNPostalAddress cnPostalAddress => cnPostalAddress -> IO (Id NSString)
street cnPostalAddress =
  sendMessage cnPostalAddress streetSelector

-- | @- subLocality@
subLocality :: IsCNPostalAddress cnPostalAddress => cnPostalAddress -> IO RawId
subLocality cnPostalAddress =
  sendMessage cnPostalAddress subLocalitySelector

-- | @- city@
city :: IsCNPostalAddress cnPostalAddress => cnPostalAddress -> IO (Id NSString)
city cnPostalAddress =
  sendMessage cnPostalAddress citySelector

-- | @- subAdministrativeArea@
subAdministrativeArea :: IsCNPostalAddress cnPostalAddress => cnPostalAddress -> IO RawId
subAdministrativeArea cnPostalAddress =
  sendMessage cnPostalAddress subAdministrativeAreaSelector

-- | @- state@
state :: IsCNPostalAddress cnPostalAddress => cnPostalAddress -> IO (Id NSString)
state cnPostalAddress =
  sendMessage cnPostalAddress stateSelector

-- | @- postalCode@
postalCode :: IsCNPostalAddress cnPostalAddress => cnPostalAddress -> IO (Id NSString)
postalCode cnPostalAddress =
  sendMessage cnPostalAddress postalCodeSelector

-- | @- country@
country :: IsCNPostalAddress cnPostalAddress => cnPostalAddress -> IO (Id NSString)
country cnPostalAddress =
  sendMessage cnPostalAddress countrySelector

-- | @- ISOCountryCode@
isoCountryCode :: IsCNPostalAddress cnPostalAddress => cnPostalAddress -> IO (Id NSString)
isoCountryCode cnPostalAddress =
  sendMessage cnPostalAddress isoCountryCodeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @localizedStringForKey:@
localizedStringForKeySelector :: Selector '[Id NSString] (Id NSString)
localizedStringForKeySelector = mkSelector "localizedStringForKey:"

-- | @Selector@ for @street@
streetSelector :: Selector '[] (Id NSString)
streetSelector = mkSelector "street"

-- | @Selector@ for @subLocality@
subLocalitySelector :: Selector '[] RawId
subLocalitySelector = mkSelector "subLocality"

-- | @Selector@ for @city@
citySelector :: Selector '[] (Id NSString)
citySelector = mkSelector "city"

-- | @Selector@ for @subAdministrativeArea@
subAdministrativeAreaSelector :: Selector '[] RawId
subAdministrativeAreaSelector = mkSelector "subAdministrativeArea"

-- | @Selector@ for @state@
stateSelector :: Selector '[] (Id NSString)
stateSelector = mkSelector "state"

-- | @Selector@ for @postalCode@
postalCodeSelector :: Selector '[] (Id NSString)
postalCodeSelector = mkSelector "postalCode"

-- | @Selector@ for @country@
countrySelector :: Selector '[] (Id NSString)
countrySelector = mkSelector "country"

-- | @Selector@ for @ISOCountryCode@
isoCountryCodeSelector :: Selector '[] (Id NSString)
isoCountryCodeSelector = mkSelector "ISOCountryCode"

