{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INTrainTrip@.
module ObjC.Intents.INTrainTrip
  ( INTrainTrip
  , IsINTrainTrip(..)
  , init_
  , initWithProvider_trainName_trainNumber_tripDuration_departureStationLocation_departurePlatform_arrivalStationLocation_arrivalPlatform
  , provider
  , trainName
  , trainNumber
  , tripDuration
  , departureStationLocation
  , departurePlatform
  , arrivalStationLocation
  , arrivalPlatform
  , arrivalPlatformSelector
  , arrivalStationLocationSelector
  , departurePlatformSelector
  , departureStationLocationSelector
  , initSelector
  , initWithProvider_trainName_trainNumber_tripDuration_departureStationLocation_departurePlatform_arrivalStationLocation_arrivalPlatformSelector
  , providerSelector
  , trainNameSelector
  , trainNumberSelector
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
init_ :: IsINTrainTrip inTrainTrip => inTrainTrip -> IO (Id INTrainTrip)
init_ inTrainTrip =
  sendOwnedMessage inTrainTrip initSelector

-- | @- initWithProvider:trainName:trainNumber:tripDuration:departureStationLocation:departurePlatform:arrivalStationLocation:arrivalPlatform:@
initWithProvider_trainName_trainNumber_tripDuration_departureStationLocation_departurePlatform_arrivalStationLocation_arrivalPlatform :: (IsINTrainTrip inTrainTrip, IsNSString provider, IsNSString trainName, IsNSString trainNumber, IsINDateComponentsRange tripDuration, IsCLPlacemark departureStationLocation, IsNSString departurePlatform, IsCLPlacemark arrivalStationLocation, IsNSString arrivalPlatform) => inTrainTrip -> provider -> trainName -> trainNumber -> tripDuration -> departureStationLocation -> departurePlatform -> arrivalStationLocation -> arrivalPlatform -> IO (Id INTrainTrip)
initWithProvider_trainName_trainNumber_tripDuration_departureStationLocation_departurePlatform_arrivalStationLocation_arrivalPlatform inTrainTrip provider trainName trainNumber tripDuration departureStationLocation departurePlatform arrivalStationLocation arrivalPlatform =
  sendOwnedMessage inTrainTrip initWithProvider_trainName_trainNumber_tripDuration_departureStationLocation_departurePlatform_arrivalStationLocation_arrivalPlatformSelector (toNSString provider) (toNSString trainName) (toNSString trainNumber) (toINDateComponentsRange tripDuration) (toCLPlacemark departureStationLocation) (toNSString departurePlatform) (toCLPlacemark arrivalStationLocation) (toNSString arrivalPlatform)

-- | @- provider@
provider :: IsINTrainTrip inTrainTrip => inTrainTrip -> IO (Id NSString)
provider inTrainTrip =
  sendMessage inTrainTrip providerSelector

-- | @- trainName@
trainName :: IsINTrainTrip inTrainTrip => inTrainTrip -> IO (Id NSString)
trainName inTrainTrip =
  sendMessage inTrainTrip trainNameSelector

-- | @- trainNumber@
trainNumber :: IsINTrainTrip inTrainTrip => inTrainTrip -> IO (Id NSString)
trainNumber inTrainTrip =
  sendMessage inTrainTrip trainNumberSelector

-- | @- tripDuration@
tripDuration :: IsINTrainTrip inTrainTrip => inTrainTrip -> IO (Id INDateComponentsRange)
tripDuration inTrainTrip =
  sendMessage inTrainTrip tripDurationSelector

-- | @- departureStationLocation@
departureStationLocation :: IsINTrainTrip inTrainTrip => inTrainTrip -> IO (Id CLPlacemark)
departureStationLocation inTrainTrip =
  sendMessage inTrainTrip departureStationLocationSelector

-- | @- departurePlatform@
departurePlatform :: IsINTrainTrip inTrainTrip => inTrainTrip -> IO (Id NSString)
departurePlatform inTrainTrip =
  sendMessage inTrainTrip departurePlatformSelector

-- | @- arrivalStationLocation@
arrivalStationLocation :: IsINTrainTrip inTrainTrip => inTrainTrip -> IO (Id CLPlacemark)
arrivalStationLocation inTrainTrip =
  sendMessage inTrainTrip arrivalStationLocationSelector

-- | @- arrivalPlatform@
arrivalPlatform :: IsINTrainTrip inTrainTrip => inTrainTrip -> IO (Id NSString)
arrivalPlatform inTrainTrip =
  sendMessage inTrainTrip arrivalPlatformSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INTrainTrip)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithProvider:trainName:trainNumber:tripDuration:departureStationLocation:departurePlatform:arrivalStationLocation:arrivalPlatform:@
initWithProvider_trainName_trainNumber_tripDuration_departureStationLocation_departurePlatform_arrivalStationLocation_arrivalPlatformSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id INDateComponentsRange, Id CLPlacemark, Id NSString, Id CLPlacemark, Id NSString] (Id INTrainTrip)
initWithProvider_trainName_trainNumber_tripDuration_departureStationLocation_departurePlatform_arrivalStationLocation_arrivalPlatformSelector = mkSelector "initWithProvider:trainName:trainNumber:tripDuration:departureStationLocation:departurePlatform:arrivalStationLocation:arrivalPlatform:"

-- | @Selector@ for @provider@
providerSelector :: Selector '[] (Id NSString)
providerSelector = mkSelector "provider"

-- | @Selector@ for @trainName@
trainNameSelector :: Selector '[] (Id NSString)
trainNameSelector = mkSelector "trainName"

-- | @Selector@ for @trainNumber@
trainNumberSelector :: Selector '[] (Id NSString)
trainNumberSelector = mkSelector "trainNumber"

-- | @Selector@ for @tripDuration@
tripDurationSelector :: Selector '[] (Id INDateComponentsRange)
tripDurationSelector = mkSelector "tripDuration"

-- | @Selector@ for @departureStationLocation@
departureStationLocationSelector :: Selector '[] (Id CLPlacemark)
departureStationLocationSelector = mkSelector "departureStationLocation"

-- | @Selector@ for @departurePlatform@
departurePlatformSelector :: Selector '[] (Id NSString)
departurePlatformSelector = mkSelector "departurePlatform"

-- | @Selector@ for @arrivalStationLocation@
arrivalStationLocationSelector :: Selector '[] (Id CLPlacemark)
arrivalStationLocationSelector = mkSelector "arrivalStationLocation"

-- | @Selector@ for @arrivalPlatform@
arrivalPlatformSelector :: Selector '[] (Id NSString)
arrivalPlatformSelector = mkSelector "arrivalPlatform"

