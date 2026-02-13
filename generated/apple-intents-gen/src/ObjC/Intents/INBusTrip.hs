{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INBusTrip@.
module ObjC.Intents.INBusTrip
  ( INBusTrip
  , IsINBusTrip(..)
  , init_
  , initWithProvider_busName_busNumber_tripDuration_departureBusStopLocation_departurePlatform_arrivalBusStopLocation_arrivalPlatform
  , provider
  , busName
  , busNumber
  , tripDuration
  , departureBusStopLocation
  , departurePlatform
  , arrivalBusStopLocation
  , arrivalPlatform
  , arrivalBusStopLocationSelector
  , arrivalPlatformSelector
  , busNameSelector
  , busNumberSelector
  , departureBusStopLocationSelector
  , departurePlatformSelector
  , initSelector
  , initWithProvider_busName_busNumber_tripDuration_departureBusStopLocation_departurePlatform_arrivalBusStopLocation_arrivalPlatformSelector
  , providerSelector
  , tripDurationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINBusTrip inBusTrip => inBusTrip -> IO (Id INBusTrip)
init_ inBusTrip =
  sendOwnedMessage inBusTrip initSelector

-- | @- initWithProvider:busName:busNumber:tripDuration:departureBusStopLocation:departurePlatform:arrivalBusStopLocation:arrivalPlatform:@
initWithProvider_busName_busNumber_tripDuration_departureBusStopLocation_departurePlatform_arrivalBusStopLocation_arrivalPlatform :: (IsINBusTrip inBusTrip, IsNSString provider, IsNSString busName, IsNSString busNumber, IsINDateComponentsRange tripDuration, IsCLPlacemark departureBusStopLocation, IsNSString departurePlatform, IsCLPlacemark arrivalBusStopLocation, IsNSString arrivalPlatform) => inBusTrip -> provider -> busName -> busNumber -> tripDuration -> departureBusStopLocation -> departurePlatform -> arrivalBusStopLocation -> arrivalPlatform -> IO (Id INBusTrip)
initWithProvider_busName_busNumber_tripDuration_departureBusStopLocation_departurePlatform_arrivalBusStopLocation_arrivalPlatform inBusTrip provider busName busNumber tripDuration departureBusStopLocation departurePlatform arrivalBusStopLocation arrivalPlatform =
  sendOwnedMessage inBusTrip initWithProvider_busName_busNumber_tripDuration_departureBusStopLocation_departurePlatform_arrivalBusStopLocation_arrivalPlatformSelector (toNSString provider) (toNSString busName) (toNSString busNumber) (toINDateComponentsRange tripDuration) (toCLPlacemark departureBusStopLocation) (toNSString departurePlatform) (toCLPlacemark arrivalBusStopLocation) (toNSString arrivalPlatform)

-- | @- provider@
provider :: IsINBusTrip inBusTrip => inBusTrip -> IO (Id NSString)
provider inBusTrip =
  sendMessage inBusTrip providerSelector

-- | @- busName@
busName :: IsINBusTrip inBusTrip => inBusTrip -> IO (Id NSString)
busName inBusTrip =
  sendMessage inBusTrip busNameSelector

-- | @- busNumber@
busNumber :: IsINBusTrip inBusTrip => inBusTrip -> IO (Id NSString)
busNumber inBusTrip =
  sendMessage inBusTrip busNumberSelector

-- | @- tripDuration@
tripDuration :: IsINBusTrip inBusTrip => inBusTrip -> IO (Id INDateComponentsRange)
tripDuration inBusTrip =
  sendMessage inBusTrip tripDurationSelector

-- | @- departureBusStopLocation@
departureBusStopLocation :: IsINBusTrip inBusTrip => inBusTrip -> IO (Id CLPlacemark)
departureBusStopLocation inBusTrip =
  sendMessage inBusTrip departureBusStopLocationSelector

-- | @- departurePlatform@
departurePlatform :: IsINBusTrip inBusTrip => inBusTrip -> IO (Id NSString)
departurePlatform inBusTrip =
  sendMessage inBusTrip departurePlatformSelector

-- | @- arrivalBusStopLocation@
arrivalBusStopLocation :: IsINBusTrip inBusTrip => inBusTrip -> IO (Id CLPlacemark)
arrivalBusStopLocation inBusTrip =
  sendMessage inBusTrip arrivalBusStopLocationSelector

-- | @- arrivalPlatform@
arrivalPlatform :: IsINBusTrip inBusTrip => inBusTrip -> IO (Id NSString)
arrivalPlatform inBusTrip =
  sendMessage inBusTrip arrivalPlatformSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INBusTrip)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithProvider:busName:busNumber:tripDuration:departureBusStopLocation:departurePlatform:arrivalBusStopLocation:arrivalPlatform:@
initWithProvider_busName_busNumber_tripDuration_departureBusStopLocation_departurePlatform_arrivalBusStopLocation_arrivalPlatformSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id INDateComponentsRange, Id CLPlacemark, Id NSString, Id CLPlacemark, Id NSString] (Id INBusTrip)
initWithProvider_busName_busNumber_tripDuration_departureBusStopLocation_departurePlatform_arrivalBusStopLocation_arrivalPlatformSelector = mkSelector "initWithProvider:busName:busNumber:tripDuration:departureBusStopLocation:departurePlatform:arrivalBusStopLocation:arrivalPlatform:"

-- | @Selector@ for @provider@
providerSelector :: Selector '[] (Id NSString)
providerSelector = mkSelector "provider"

-- | @Selector@ for @busName@
busNameSelector :: Selector '[] (Id NSString)
busNameSelector = mkSelector "busName"

-- | @Selector@ for @busNumber@
busNumberSelector :: Selector '[] (Id NSString)
busNumberSelector = mkSelector "busNumber"

-- | @Selector@ for @tripDuration@
tripDurationSelector :: Selector '[] (Id INDateComponentsRange)
tripDurationSelector = mkSelector "tripDuration"

-- | @Selector@ for @departureBusStopLocation@
departureBusStopLocationSelector :: Selector '[] (Id CLPlacemark)
departureBusStopLocationSelector = mkSelector "departureBusStopLocation"

-- | @Selector@ for @departurePlatform@
departurePlatformSelector :: Selector '[] (Id NSString)
departurePlatformSelector = mkSelector "departurePlatform"

-- | @Selector@ for @arrivalBusStopLocation@
arrivalBusStopLocationSelector :: Selector '[] (Id CLPlacemark)
arrivalBusStopLocationSelector = mkSelector "arrivalBusStopLocation"

-- | @Selector@ for @arrivalPlatform@
arrivalPlatformSelector :: Selector '[] (Id NSString)
arrivalPlatformSelector = mkSelector "arrivalPlatform"

