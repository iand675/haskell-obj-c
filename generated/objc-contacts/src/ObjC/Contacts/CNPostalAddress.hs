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
  , city
  , state
  , postalCode
  , country
  , isoCountryCode
  , localizedStringForKeySelector
  , streetSelector
  , citySelector
  , stateSelector
  , postalCodeSelector
  , countrySelector
  , isoCountryCodeSelector


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

-- | Returns a user displayable property name.
--
-- ObjC selector: @+ localizedStringForKey:@
localizedStringForKey :: IsNSString key => key -> IO (Id NSString)
localizedStringForKey key =
  do
    cls' <- getRequiredClass "CNPostalAddress"
    withObjCPtr key $ \raw_key ->
      sendClassMsg cls' (mkSelector "localizedStringForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | multi-street address is delimited with carriage returns “”
--
-- ObjC selector: @- street@
street :: IsCNPostalAddress cnPostalAddress => cnPostalAddress -> IO (Id NSString)
street cnPostalAddress  =
  sendMsg cnPostalAddress (mkSelector "street") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- city@
city :: IsCNPostalAddress cnPostalAddress => cnPostalAddress -> IO (Id NSString)
city cnPostalAddress  =
  sendMsg cnPostalAddress (mkSelector "city") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- state@
state :: IsCNPostalAddress cnPostalAddress => cnPostalAddress -> IO (Id NSString)
state cnPostalAddress  =
  sendMsg cnPostalAddress (mkSelector "state") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- postalCode@
postalCode :: IsCNPostalAddress cnPostalAddress => cnPostalAddress -> IO (Id NSString)
postalCode cnPostalAddress  =
  sendMsg cnPostalAddress (mkSelector "postalCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- country@
country :: IsCNPostalAddress cnPostalAddress => cnPostalAddress -> IO (Id NSString)
country cnPostalAddress  =
  sendMsg cnPostalAddress (mkSelector "country") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- ISOCountryCode@
isoCountryCode :: IsCNPostalAddress cnPostalAddress => cnPostalAddress -> IO (Id NSString)
isoCountryCode cnPostalAddress  =
  sendMsg cnPostalAddress (mkSelector "ISOCountryCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @localizedStringForKey:@
localizedStringForKeySelector :: Selector
localizedStringForKeySelector = mkSelector "localizedStringForKey:"

-- | @Selector@ for @street@
streetSelector :: Selector
streetSelector = mkSelector "street"

-- | @Selector@ for @city@
citySelector :: Selector
citySelector = mkSelector "city"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @postalCode@
postalCodeSelector :: Selector
postalCodeSelector = mkSelector "postalCode"

-- | @Selector@ for @country@
countrySelector :: Selector
countrySelector = mkSelector "country"

-- | @Selector@ for @ISOCountryCode@
isoCountryCodeSelector :: Selector
isoCountryCodeSelector = mkSelector "ISOCountryCode"

