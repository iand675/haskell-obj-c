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
  , initSelector
  , newSelector
  , initWithPlacemarkSelector
  , regionSelector
  , timeZoneSelector
  , addressDictionarySelector
  , nameSelector
  , thoroughfareSelector
  , subThoroughfareSelector
  , localitySelector
  , subLocalitySelector
  , administrativeAreaSelector
  , subAdministrativeAreaSelector
  , postalCodeSelector
  , isOcountryCodeSelector
  , countrySelector
  , inlandWaterSelector
  , oceanSelector
  , areasOfInterestSelector
  , postalAddressSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id CLPlacemark)
init_ clPlacemark  =
    sendMsg clPlacemark (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CLPlacemark)
new  =
  do
    cls' <- getRequiredClass "CLPlacemark"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithPlacemark:@
initWithPlacemark :: (IsCLPlacemark clPlacemark, IsCLPlacemark placemark) => clPlacemark -> placemark -> IO (Id CLPlacemark)
initWithPlacemark clPlacemark  placemark =
  withObjCPtr placemark $ \raw_placemark ->
      sendMsg clPlacemark (mkSelector "initWithPlacemark:") (retPtr retVoid) [argPtr (castPtr raw_placemark :: Ptr ())] >>= ownedObject . castPtr

-- | @- region@
region :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id CLRegion)
region clPlacemark  =
    sendMsg clPlacemark (mkSelector "region") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- timeZone@
timeZone :: IsCLPlacemark clPlacemark => clPlacemark -> IO RawId
timeZone clPlacemark  =
    fmap (RawId . castPtr) $ sendMsg clPlacemark (mkSelector "timeZone") (retPtr retVoid) []

-- | @- addressDictionary@
addressDictionary :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSDictionary)
addressDictionary clPlacemark  =
    sendMsg clPlacemark (mkSelector "addressDictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- name@
name :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
name clPlacemark  =
    sendMsg clPlacemark (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- thoroughfare@
thoroughfare :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
thoroughfare clPlacemark  =
    sendMsg clPlacemark (mkSelector "thoroughfare") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- subThoroughfare@
subThoroughfare :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
subThoroughfare clPlacemark  =
    sendMsg clPlacemark (mkSelector "subThoroughfare") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- locality@
locality :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
locality clPlacemark  =
    sendMsg clPlacemark (mkSelector "locality") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- subLocality@
subLocality :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
subLocality clPlacemark  =
    sendMsg clPlacemark (mkSelector "subLocality") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- administrativeArea@
administrativeArea :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
administrativeArea clPlacemark  =
    sendMsg clPlacemark (mkSelector "administrativeArea") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- subAdministrativeArea@
subAdministrativeArea :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
subAdministrativeArea clPlacemark  =
    sendMsg clPlacemark (mkSelector "subAdministrativeArea") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- postalCode@
postalCode :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
postalCode clPlacemark  =
    sendMsg clPlacemark (mkSelector "postalCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- ISOcountryCode@
isOcountryCode :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
isOcountryCode clPlacemark  =
    sendMsg clPlacemark (mkSelector "ISOcountryCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- country@
country :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
country clPlacemark  =
    sendMsg clPlacemark (mkSelector "country") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- inlandWater@
inlandWater :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
inlandWater clPlacemark  =
    sendMsg clPlacemark (mkSelector "inlandWater") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- ocean@
ocean :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSString)
ocean clPlacemark  =
    sendMsg clPlacemark (mkSelector "ocean") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- areasOfInterest@
areasOfInterest :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id NSArray)
areasOfInterest clPlacemark  =
    sendMsg clPlacemark (mkSelector "areasOfInterest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- postalAddress@
postalAddress :: IsCLPlacemark clPlacemark => clPlacemark -> IO (Id CNPostalAddress)
postalAddress clPlacemark  =
    sendMsg clPlacemark (mkSelector "postalAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithPlacemark:@
initWithPlacemarkSelector :: Selector
initWithPlacemarkSelector = mkSelector "initWithPlacemark:"

-- | @Selector@ for @region@
regionSelector :: Selector
regionSelector = mkSelector "region"

-- | @Selector@ for @timeZone@
timeZoneSelector :: Selector
timeZoneSelector = mkSelector "timeZone"

-- | @Selector@ for @addressDictionary@
addressDictionarySelector :: Selector
addressDictionarySelector = mkSelector "addressDictionary"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @thoroughfare@
thoroughfareSelector :: Selector
thoroughfareSelector = mkSelector "thoroughfare"

-- | @Selector@ for @subThoroughfare@
subThoroughfareSelector :: Selector
subThoroughfareSelector = mkSelector "subThoroughfare"

-- | @Selector@ for @locality@
localitySelector :: Selector
localitySelector = mkSelector "locality"

-- | @Selector@ for @subLocality@
subLocalitySelector :: Selector
subLocalitySelector = mkSelector "subLocality"

-- | @Selector@ for @administrativeArea@
administrativeAreaSelector :: Selector
administrativeAreaSelector = mkSelector "administrativeArea"

-- | @Selector@ for @subAdministrativeArea@
subAdministrativeAreaSelector :: Selector
subAdministrativeAreaSelector = mkSelector "subAdministrativeArea"

-- | @Selector@ for @postalCode@
postalCodeSelector :: Selector
postalCodeSelector = mkSelector "postalCode"

-- | @Selector@ for @ISOcountryCode@
isOcountryCodeSelector :: Selector
isOcountryCodeSelector = mkSelector "ISOcountryCode"

-- | @Selector@ for @country@
countrySelector :: Selector
countrySelector = mkSelector "country"

-- | @Selector@ for @inlandWater@
inlandWaterSelector :: Selector
inlandWaterSelector = mkSelector "inlandWater"

-- | @Selector@ for @ocean@
oceanSelector :: Selector
oceanSelector = mkSelector "ocean"

-- | @Selector@ for @areasOfInterest@
areasOfInterestSelector :: Selector
areasOfInterestSelector = mkSelector "areasOfInterest"

-- | @Selector@ for @postalAddress@
postalAddressSelector :: Selector
postalAddressSelector = mkSelector "postalAddress"

