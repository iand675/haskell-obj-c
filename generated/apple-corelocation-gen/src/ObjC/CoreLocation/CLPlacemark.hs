{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLPlacemark@.
module ObjC.CoreLocation.CLPlacemark
  ( CLPlacemark
  , IsCLPlacemark(..)
  , init_
  , new
  , initWithPlacemark
  , region
  , timeZone
  , addressDictionary
  , name
  , thoroughfare
  , subThoroughfare
  , locality
  , subLocality
  , administrativeArea
  , subAdministrativeArea
  , postalCode
  , isOcountryCode
  , country
  , inlandWater
  , ocean
  , areasOfInterest
  , postalAddress
  , addressDictionarySelector
  , administrativeAreaSelector
  , areasOfInterestSelector
  , countrySelector
  , initSelector
  , initWithPlacemarkSelector
  , inlandWaterSelector
  , isOcountryCodeSelector
  , localitySelector
  , nameSelector
  , newSelector
  , oceanSelector
  , postalAddressSelector
  , postalCodeSelector
  , regionSelector
  , subAdministrativeAreaSelector
  , subLocalitySelector
  , subThoroughfareSelector
  , thoroughfareSelector
  , timeZoneSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id CLPlacemark)
init_ clPlacemark =
  sendOwnedMessage clPlacemark initSelector

-- | @+ new@
new :: IO (Id CLPlacemark)
new  =
  do
    cls' <- getRequiredClass "CLPlacemark"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithPlacemark:@
initWithPlacemark :: (IsCLPlacemark clPlacemark, IsCLPlacemark placemark) => clPlacemark -> placemark -> IO (Id CLPlacemark)
initWithPlacemark clPlacemark placemark =
  sendOwnedMessage clPlacemark initWithPlacemarkSelector (toCLPlacemark placemark)

-- | @- region@
region :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id CLRegion)
region clPlacemark =
  sendMessage clPlacemark regionSelector

-- | @- timeZone@
timeZone :: IsCLPlacemark clPlacemark => clPlacemark -> IO RawId
timeZone clPlacemark =
  sendMessage clPlacemark timeZoneSelector

-- | @- addressDictionary@
addressDictionary :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSDictionary)
addressDictionary clPlacemark =
  sendMessage clPlacemark addressDictionarySelector

-- | @- name@
name :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
name clPlacemark =
  sendMessage clPlacemark nameSelector

-- | @- thoroughfare@
thoroughfare :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
thoroughfare clPlacemark =
  sendMessage clPlacemark thoroughfareSelector

-- | @- subThoroughfare@
subThoroughfare :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
subThoroughfare clPlacemark =
  sendMessage clPlacemark subThoroughfareSelector

-- | @- locality@
locality :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
locality clPlacemark =
  sendMessage clPlacemark localitySelector

-- | @- subLocality@
subLocality :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
subLocality clPlacemark =
  sendMessage clPlacemark subLocalitySelector

-- | @- administrativeArea@
administrativeArea :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
administrativeArea clPlacemark =
  sendMessage clPlacemark administrativeAreaSelector

-- | @- subAdministrativeArea@
subAdministrativeArea :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
subAdministrativeArea clPlacemark =
  sendMessage clPlacemark subAdministrativeAreaSelector

-- | @- postalCode@
postalCode :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
postalCode clPlacemark =
  sendMessage clPlacemark postalCodeSelector

-- | @- ISOcountryCode@
isOcountryCode :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
isOcountryCode clPlacemark =
  sendMessage clPlacemark isOcountryCodeSelector

-- | @- country@
country :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
country clPlacemark =
  sendMessage clPlacemark countrySelector

-- | @- inlandWater@
inlandWater :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
inlandWater clPlacemark =
  sendMessage clPlacemark inlandWaterSelector

-- | @- ocean@
ocean :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
ocean clPlacemark =
  sendMessage clPlacemark oceanSelector

-- | @- areasOfInterest@
areasOfInterest :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSArray)
areasOfInterest clPlacemark =
  sendMessage clPlacemark areasOfInterestSelector

-- | @- postalAddress@
postalAddress :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id CNPostalAddress)
postalAddress clPlacemark =
  sendMessage clPlacemark postalAddressSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CLPlacemark)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CLPlacemark)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithPlacemark:@
initWithPlacemarkSelector :: Selector '[Id CLPlacemark] (Id CLPlacemark)
initWithPlacemarkSelector = mkSelector "initWithPlacemark:"

-- | @Selector@ for @region@
regionSelector :: Selector '[] (Id CLRegion)
regionSelector = mkSelector "region"

-- | @Selector@ for @timeZone@
timeZoneSelector :: Selector '[] RawId
timeZoneSelector = mkSelector "timeZone"

-- | @Selector@ for @addressDictionary@
addressDictionarySelector :: Selector '[] (Id NSDictionary)
addressDictionarySelector = mkSelector "addressDictionary"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @thoroughfare@
thoroughfareSelector :: Selector '[] (Id NSString)
thoroughfareSelector = mkSelector "thoroughfare"

-- | @Selector@ for @subThoroughfare@
subThoroughfareSelector :: Selector '[] (Id NSString)
subThoroughfareSelector = mkSelector "subThoroughfare"

-- | @Selector@ for @locality@
localitySelector :: Selector '[] (Id NSString)
localitySelector = mkSelector "locality"

-- | @Selector@ for @subLocality@
subLocalitySelector :: Selector '[] (Id NSString)
subLocalitySelector = mkSelector "subLocality"

-- | @Selector@ for @administrativeArea@
administrativeAreaSelector :: Selector '[] (Id NSString)
administrativeAreaSelector = mkSelector "administrativeArea"

-- | @Selector@ for @subAdministrativeArea@
subAdministrativeAreaSelector :: Selector '[] (Id NSString)
subAdministrativeAreaSelector = mkSelector "subAdministrativeArea"

-- | @Selector@ for @postalCode@
postalCodeSelector :: Selector '[] (Id NSString)
postalCodeSelector = mkSelector "postalCode"

-- | @Selector@ for @ISOcountryCode@
isOcountryCodeSelector :: Selector '[] (Id NSString)
isOcountryCodeSelector = mkSelector "ISOcountryCode"

-- | @Selector@ for @country@
countrySelector :: Selector '[] (Id NSString)
countrySelector = mkSelector "country"

-- | @Selector@ for @inlandWater@
inlandWaterSelector :: Selector '[] (Id NSString)
inlandWaterSelector = mkSelector "inlandWater"

-- | @Selector@ for @ocean@
oceanSelector :: Selector '[] (Id NSString)
oceanSelector = mkSelector "ocean"

-- | @Selector@ for @areasOfInterest@
areasOfInterestSelector :: Selector '[] (Id NSArray)
areasOfInterestSelector = mkSelector "areasOfInterest"

-- | @Selector@ for @postalAddress@
postalAddressSelector :: Selector '[] (Id CNPostalAddress)
postalAddressSelector = mkSelector "postalAddress"

