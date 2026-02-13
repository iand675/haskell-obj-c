{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that contains a postal address that the data detection system matches.
--
-- The DataDetection framework returns a postal address match in a @DDMatchPostalAddress@ object, which optionally contains the matching parts of a postal address: street, city, state, postal code, and country.
--
-- Generated bindings for @DDMatchPostalAddress@.
module ObjC.DataDetection.DDMatchPostalAddress
  ( DDMatchPostalAddress
  , IsDDMatchPostalAddress(..)
  , street
  , city
  , state
  , postalCode
  , country
  , citySelector
  , countrySelector
  , postalCodeSelector
  , stateSelector
  , streetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.DataDetection.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The street name in a postal address.
--
-- ObjC selector: @- street@
street :: IsDDMatchPostalAddress ddMatchPostalAddress => ddMatchPostalAddress -> IO (Id NSString)
street ddMatchPostalAddress =
  sendMessage ddMatchPostalAddress streetSelector

-- | The city name in a postal address.
--
-- ObjC selector: @- city@
city :: IsDDMatchPostalAddress ddMatchPostalAddress => ddMatchPostalAddress -> IO (Id NSString)
city ddMatchPostalAddress =
  sendMessage ddMatchPostalAddress citySelector

-- | The state name in a postal address.
--
-- ObjC selector: @- state@
state :: IsDDMatchPostalAddress ddMatchPostalAddress => ddMatchPostalAddress -> IO (Id NSString)
state ddMatchPostalAddress =
  sendMessage ddMatchPostalAddress stateSelector

-- | The postal code in a postal address.
--
-- ObjC selector: @- postalCode@
postalCode :: IsDDMatchPostalAddress ddMatchPostalAddress => ddMatchPostalAddress -> IO (Id NSString)
postalCode ddMatchPostalAddress =
  sendMessage ddMatchPostalAddress postalCodeSelector

-- | The country or region name in a postal address.
--
-- ObjC selector: @- country@
country :: IsDDMatchPostalAddress ddMatchPostalAddress => ddMatchPostalAddress -> IO (Id NSString)
country ddMatchPostalAddress =
  sendMessage ddMatchPostalAddress countrySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @street@
streetSelector :: Selector '[] (Id NSString)
streetSelector = mkSelector "street"

-- | @Selector@ for @city@
citySelector :: Selector '[] (Id NSString)
citySelector = mkSelector "city"

-- | @Selector@ for @state@
stateSelector :: Selector '[] (Id NSString)
stateSelector = mkSelector "state"

-- | @Selector@ for @postalCode@
postalCodeSelector :: Selector '[] (Id NSString)
postalCodeSelector = mkSelector "postalCode"

-- | @Selector@ for @country@
countrySelector :: Selector '[] (Id NSString)
countrySelector = mkSelector "country"

