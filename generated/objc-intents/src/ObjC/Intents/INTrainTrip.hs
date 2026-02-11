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
  , initSelector
  , initWithProvider_trainName_trainNumber_tripDuration_departureStationLocation_departurePlatform_arrivalStationLocation_arrivalPlatformSelector
  , providerSelector
  , trainNameSelector
  , trainNumberSelector
  , tripDurationSelector
  , departureStationLocationSelector
  , departurePlatformSelector
  , arrivalStationLocationSelector
  , arrivalPlatformSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINTrainTrip inTrainTrip => inTrainTrip -> IO (Id INTrainTrip)
init_ inTrainTrip  =
  sendMsg inTrainTrip (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithProvider:trainName:trainNumber:tripDuration:departureStationLocation:departurePlatform:arrivalStationLocation:arrivalPlatform:@
initWithProvider_trainName_trainNumber_tripDuration_departureStationLocation_departurePlatform_arrivalStationLocation_arrivalPlatform :: (IsINTrainTrip inTrainTrip, IsNSString provider, IsNSString trainName, IsNSString trainNumber, IsINDateComponentsRange tripDuration, IsCLPlacemark departureStationLocation, IsNSString departurePlatform, IsCLPlacemark arrivalStationLocation, IsNSString arrivalPlatform) => inTrainTrip -> provider -> trainName -> trainNumber -> tripDuration -> departureStationLocation -> departurePlatform -> arrivalStationLocation -> arrivalPlatform -> IO (Id INTrainTrip)
initWithProvider_trainName_trainNumber_tripDuration_departureStationLocation_departurePlatform_arrivalStationLocation_arrivalPlatform inTrainTrip  provider trainName trainNumber tripDuration departureStationLocation departurePlatform arrivalStationLocation arrivalPlatform =
withObjCPtr provider $ \raw_provider ->
  withObjCPtr trainName $ \raw_trainName ->
    withObjCPtr trainNumber $ \raw_trainNumber ->
      withObjCPtr tripDuration $ \raw_tripDuration ->
        withObjCPtr departureStationLocation $ \raw_departureStationLocation ->
          withObjCPtr departurePlatform $ \raw_departurePlatform ->
            withObjCPtr arrivalStationLocation $ \raw_arrivalStationLocation ->
              withObjCPtr arrivalPlatform $ \raw_arrivalPlatform ->
                  sendMsg inTrainTrip (mkSelector "initWithProvider:trainName:trainNumber:tripDuration:departureStationLocation:departurePlatform:arrivalStationLocation:arrivalPlatform:") (retPtr retVoid) [argPtr (castPtr raw_provider :: Ptr ()), argPtr (castPtr raw_trainName :: Ptr ()), argPtr (castPtr raw_trainNumber :: Ptr ()), argPtr (castPtr raw_tripDuration :: Ptr ()), argPtr (castPtr raw_departureStationLocation :: Ptr ()), argPtr (castPtr raw_departurePlatform :: Ptr ()), argPtr (castPtr raw_arrivalStationLocation :: Ptr ()), argPtr (castPtr raw_arrivalPlatform :: Ptr ())] >>= ownedObject . castPtr

-- | @- provider@
provider :: IsINTrainTrip inTrainTrip => inTrainTrip -> IO (Id NSString)
provider inTrainTrip  =
  sendMsg inTrainTrip (mkSelector "provider") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- trainName@
trainName :: IsINTrainTrip inTrainTrip => inTrainTrip -> IO (Id NSString)
trainName inTrainTrip  =
  sendMsg inTrainTrip (mkSelector "trainName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- trainNumber@
trainNumber :: IsINTrainTrip inTrainTrip => inTrainTrip -> IO (Id NSString)
trainNumber inTrainTrip  =
  sendMsg inTrainTrip (mkSelector "trainNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- tripDuration@
tripDuration :: IsINTrainTrip inTrainTrip => inTrainTrip -> IO (Id INDateComponentsRange)
tripDuration inTrainTrip  =
  sendMsg inTrainTrip (mkSelector "tripDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- departureStationLocation@
departureStationLocation :: IsINTrainTrip inTrainTrip => inTrainTrip -> IO (Id CLPlacemark)
departureStationLocation inTrainTrip  =
  sendMsg inTrainTrip (mkSelector "departureStationLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- departurePlatform@
departurePlatform :: IsINTrainTrip inTrainTrip => inTrainTrip -> IO (Id NSString)
departurePlatform inTrainTrip  =
  sendMsg inTrainTrip (mkSelector "departurePlatform") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- arrivalStationLocation@
arrivalStationLocation :: IsINTrainTrip inTrainTrip => inTrainTrip -> IO (Id CLPlacemark)
arrivalStationLocation inTrainTrip  =
  sendMsg inTrainTrip (mkSelector "arrivalStationLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- arrivalPlatform@
arrivalPlatform :: IsINTrainTrip inTrainTrip => inTrainTrip -> IO (Id NSString)
arrivalPlatform inTrainTrip  =
  sendMsg inTrainTrip (mkSelector "arrivalPlatform") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithProvider:trainName:trainNumber:tripDuration:departureStationLocation:departurePlatform:arrivalStationLocation:arrivalPlatform:@
initWithProvider_trainName_trainNumber_tripDuration_departureStationLocation_departurePlatform_arrivalStationLocation_arrivalPlatformSelector :: Selector
initWithProvider_trainName_trainNumber_tripDuration_departureStationLocation_departurePlatform_arrivalStationLocation_arrivalPlatformSelector = mkSelector "initWithProvider:trainName:trainNumber:tripDuration:departureStationLocation:departurePlatform:arrivalStationLocation:arrivalPlatform:"

-- | @Selector@ for @provider@
providerSelector :: Selector
providerSelector = mkSelector "provider"

-- | @Selector@ for @trainName@
trainNameSelector :: Selector
trainNameSelector = mkSelector "trainName"

-- | @Selector@ for @trainNumber@
trainNumberSelector :: Selector
trainNumberSelector = mkSelector "trainNumber"

-- | @Selector@ for @tripDuration@
tripDurationSelector :: Selector
tripDurationSelector = mkSelector "tripDuration"

-- | @Selector@ for @departureStationLocation@
departureStationLocationSelector :: Selector
departureStationLocationSelector = mkSelector "departureStationLocation"

-- | @Selector@ for @departurePlatform@
departurePlatformSelector :: Selector
departurePlatformSelector = mkSelector "departurePlatform"

-- | @Selector@ for @arrivalStationLocation@
arrivalStationLocationSelector :: Selector
arrivalStationLocationSelector = mkSelector "arrivalStationLocation"

-- | @Selector@ for @arrivalPlatform@
arrivalPlatformSelector :: Selector
arrivalPlatformSelector = mkSelector "arrivalPlatform"

