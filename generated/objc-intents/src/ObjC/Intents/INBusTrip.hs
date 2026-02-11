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
  , initSelector
  , initWithProvider_busName_busNumber_tripDuration_departureBusStopLocation_departurePlatform_arrivalBusStopLocation_arrivalPlatformSelector
  , providerSelector
  , busNameSelector
  , busNumberSelector
  , tripDurationSelector
  , departureBusStopLocationSelector
  , departurePlatformSelector
  , arrivalBusStopLocationSelector
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
init_ :: IsINBusTrip inBusTrip => inBusTrip -> IO (Id INBusTrip)
init_ inBusTrip  =
  sendMsg inBusTrip (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithProvider:busName:busNumber:tripDuration:departureBusStopLocation:departurePlatform:arrivalBusStopLocation:arrivalPlatform:@
initWithProvider_busName_busNumber_tripDuration_departureBusStopLocation_departurePlatform_arrivalBusStopLocation_arrivalPlatform :: (IsINBusTrip inBusTrip, IsNSString provider, IsNSString busName, IsNSString busNumber, IsINDateComponentsRange tripDuration, IsCLPlacemark departureBusStopLocation, IsNSString departurePlatform, IsCLPlacemark arrivalBusStopLocation, IsNSString arrivalPlatform) => inBusTrip -> provider -> busName -> busNumber -> tripDuration -> departureBusStopLocation -> departurePlatform -> arrivalBusStopLocation -> arrivalPlatform -> IO (Id INBusTrip)
initWithProvider_busName_busNumber_tripDuration_departureBusStopLocation_departurePlatform_arrivalBusStopLocation_arrivalPlatform inBusTrip  provider busName busNumber tripDuration departureBusStopLocation departurePlatform arrivalBusStopLocation arrivalPlatform =
withObjCPtr provider $ \raw_provider ->
  withObjCPtr busName $ \raw_busName ->
    withObjCPtr busNumber $ \raw_busNumber ->
      withObjCPtr tripDuration $ \raw_tripDuration ->
        withObjCPtr departureBusStopLocation $ \raw_departureBusStopLocation ->
          withObjCPtr departurePlatform $ \raw_departurePlatform ->
            withObjCPtr arrivalBusStopLocation $ \raw_arrivalBusStopLocation ->
              withObjCPtr arrivalPlatform $ \raw_arrivalPlatform ->
                  sendMsg inBusTrip (mkSelector "initWithProvider:busName:busNumber:tripDuration:departureBusStopLocation:departurePlatform:arrivalBusStopLocation:arrivalPlatform:") (retPtr retVoid) [argPtr (castPtr raw_provider :: Ptr ()), argPtr (castPtr raw_busName :: Ptr ()), argPtr (castPtr raw_busNumber :: Ptr ()), argPtr (castPtr raw_tripDuration :: Ptr ()), argPtr (castPtr raw_departureBusStopLocation :: Ptr ()), argPtr (castPtr raw_departurePlatform :: Ptr ()), argPtr (castPtr raw_arrivalBusStopLocation :: Ptr ()), argPtr (castPtr raw_arrivalPlatform :: Ptr ())] >>= ownedObject . castPtr

-- | @- provider@
provider :: IsINBusTrip inBusTrip => inBusTrip -> IO (Id NSString)
provider inBusTrip  =
  sendMsg inBusTrip (mkSelector "provider") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- busName@
busName :: IsINBusTrip inBusTrip => inBusTrip -> IO (Id NSString)
busName inBusTrip  =
  sendMsg inBusTrip (mkSelector "busName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- busNumber@
busNumber :: IsINBusTrip inBusTrip => inBusTrip -> IO (Id NSString)
busNumber inBusTrip  =
  sendMsg inBusTrip (mkSelector "busNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- tripDuration@
tripDuration :: IsINBusTrip inBusTrip => inBusTrip -> IO (Id INDateComponentsRange)
tripDuration inBusTrip  =
  sendMsg inBusTrip (mkSelector "tripDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- departureBusStopLocation@
departureBusStopLocation :: IsINBusTrip inBusTrip => inBusTrip -> IO (Id CLPlacemark)
departureBusStopLocation inBusTrip  =
  sendMsg inBusTrip (mkSelector "departureBusStopLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- departurePlatform@
departurePlatform :: IsINBusTrip inBusTrip => inBusTrip -> IO (Id NSString)
departurePlatform inBusTrip  =
  sendMsg inBusTrip (mkSelector "departurePlatform") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- arrivalBusStopLocation@
arrivalBusStopLocation :: IsINBusTrip inBusTrip => inBusTrip -> IO (Id CLPlacemark)
arrivalBusStopLocation inBusTrip  =
  sendMsg inBusTrip (mkSelector "arrivalBusStopLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- arrivalPlatform@
arrivalPlatform :: IsINBusTrip inBusTrip => inBusTrip -> IO (Id NSString)
arrivalPlatform inBusTrip  =
  sendMsg inBusTrip (mkSelector "arrivalPlatform") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithProvider:busName:busNumber:tripDuration:departureBusStopLocation:departurePlatform:arrivalBusStopLocation:arrivalPlatform:@
initWithProvider_busName_busNumber_tripDuration_departureBusStopLocation_departurePlatform_arrivalBusStopLocation_arrivalPlatformSelector :: Selector
initWithProvider_busName_busNumber_tripDuration_departureBusStopLocation_departurePlatform_arrivalBusStopLocation_arrivalPlatformSelector = mkSelector "initWithProvider:busName:busNumber:tripDuration:departureBusStopLocation:departurePlatform:arrivalBusStopLocation:arrivalPlatform:"

-- | @Selector@ for @provider@
providerSelector :: Selector
providerSelector = mkSelector "provider"

-- | @Selector@ for @busName@
busNameSelector :: Selector
busNameSelector = mkSelector "busName"

-- | @Selector@ for @busNumber@
busNumberSelector :: Selector
busNumberSelector = mkSelector "busNumber"

-- | @Selector@ for @tripDuration@
tripDurationSelector :: Selector
tripDurationSelector = mkSelector "tripDuration"

-- | @Selector@ for @departureBusStopLocation@
departureBusStopLocationSelector :: Selector
departureBusStopLocationSelector = mkSelector "departureBusStopLocation"

-- | @Selector@ for @departurePlatform@
departurePlatformSelector :: Selector
departurePlatformSelector = mkSelector "departurePlatform"

-- | @Selector@ for @arrivalBusStopLocation@
arrivalBusStopLocationSelector :: Selector
arrivalBusStopLocationSelector = mkSelector "arrivalBusStopLocation"

-- | @Selector@ for @arrivalPlatform@
arrivalPlatformSelector :: Selector
arrivalPlatformSelector = mkSelector "arrivalPlatform"

