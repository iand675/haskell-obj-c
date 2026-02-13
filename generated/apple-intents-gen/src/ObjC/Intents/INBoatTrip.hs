{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INBoatTrip@.
module ObjC.Intents.INBoatTrip
  ( INBoatTrip
  , IsINBoatTrip(..)
  , init_
  , initWithProvider_boatName_boatNumber_tripDuration_departureBoatTerminalLocation_arrivalBoatTerminalLocation
  , provider
  , boatName
  , boatNumber
  , tripDuration
  , departureBoatTerminalLocation
  , arrivalBoatTerminalLocation
  , arrivalBoatTerminalLocationSelector
  , boatNameSelector
  , boatNumberSelector
  , departureBoatTerminalLocationSelector
  , initSelector
  , initWithProvider_boatName_boatNumber_tripDuration_departureBoatTerminalLocation_arrivalBoatTerminalLocationSelector
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
init_ :: IsINBoatTrip inBoatTrip => inBoatTrip -> IO (Id INBoatTrip)
init_ inBoatTrip =
  sendOwnedMessage inBoatTrip initSelector

-- | @- initWithProvider:boatName:boatNumber:tripDuration:departureBoatTerminalLocation:arrivalBoatTerminalLocation:@
initWithProvider_boatName_boatNumber_tripDuration_departureBoatTerminalLocation_arrivalBoatTerminalLocation :: (IsINBoatTrip inBoatTrip, IsNSString provider, IsNSString boatName, IsNSString boatNumber, IsINDateComponentsRange tripDuration, IsCLPlacemark departureBoatTerminalLocation, IsCLPlacemark arrivalBoatTerminalLocation) => inBoatTrip -> provider -> boatName -> boatNumber -> tripDuration -> departureBoatTerminalLocation -> arrivalBoatTerminalLocation -> IO (Id INBoatTrip)
initWithProvider_boatName_boatNumber_tripDuration_departureBoatTerminalLocation_arrivalBoatTerminalLocation inBoatTrip provider boatName boatNumber tripDuration departureBoatTerminalLocation arrivalBoatTerminalLocation =
  sendOwnedMessage inBoatTrip initWithProvider_boatName_boatNumber_tripDuration_departureBoatTerminalLocation_arrivalBoatTerminalLocationSelector (toNSString provider) (toNSString boatName) (toNSString boatNumber) (toINDateComponentsRange tripDuration) (toCLPlacemark departureBoatTerminalLocation) (toCLPlacemark arrivalBoatTerminalLocation)

-- | @- provider@
provider :: IsINBoatTrip inBoatTrip => inBoatTrip -> IO (Id NSString)
provider inBoatTrip =
  sendMessage inBoatTrip providerSelector

-- | @- boatName@
boatName :: IsINBoatTrip inBoatTrip => inBoatTrip -> IO (Id NSString)
boatName inBoatTrip =
  sendMessage inBoatTrip boatNameSelector

-- | @- boatNumber@
boatNumber :: IsINBoatTrip inBoatTrip => inBoatTrip -> IO (Id NSString)
boatNumber inBoatTrip =
  sendMessage inBoatTrip boatNumberSelector

-- | @- tripDuration@
tripDuration :: IsINBoatTrip inBoatTrip => inBoatTrip -> IO (Id INDateComponentsRange)
tripDuration inBoatTrip =
  sendMessage inBoatTrip tripDurationSelector

-- | @- departureBoatTerminalLocation@
departureBoatTerminalLocation :: IsINBoatTrip inBoatTrip => inBoatTrip -> IO (Id CLPlacemark)
departureBoatTerminalLocation inBoatTrip =
  sendMessage inBoatTrip departureBoatTerminalLocationSelector

-- | @- arrivalBoatTerminalLocation@
arrivalBoatTerminalLocation :: IsINBoatTrip inBoatTrip => inBoatTrip -> IO (Id CLPlacemark)
arrivalBoatTerminalLocation inBoatTrip =
  sendMessage inBoatTrip arrivalBoatTerminalLocationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INBoatTrip)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithProvider:boatName:boatNumber:tripDuration:departureBoatTerminalLocation:arrivalBoatTerminalLocation:@
initWithProvider_boatName_boatNumber_tripDuration_departureBoatTerminalLocation_arrivalBoatTerminalLocationSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id INDateComponentsRange, Id CLPlacemark, Id CLPlacemark] (Id INBoatTrip)
initWithProvider_boatName_boatNumber_tripDuration_departureBoatTerminalLocation_arrivalBoatTerminalLocationSelector = mkSelector "initWithProvider:boatName:boatNumber:tripDuration:departureBoatTerminalLocation:arrivalBoatTerminalLocation:"

-- | @Selector@ for @provider@
providerSelector :: Selector '[] (Id NSString)
providerSelector = mkSelector "provider"

-- | @Selector@ for @boatName@
boatNameSelector :: Selector '[] (Id NSString)
boatNameSelector = mkSelector "boatName"

-- | @Selector@ for @boatNumber@
boatNumberSelector :: Selector '[] (Id NSString)
boatNumberSelector = mkSelector "boatNumber"

-- | @Selector@ for @tripDuration@
tripDurationSelector :: Selector '[] (Id INDateComponentsRange)
tripDurationSelector = mkSelector "tripDuration"

-- | @Selector@ for @departureBoatTerminalLocation@
departureBoatTerminalLocationSelector :: Selector '[] (Id CLPlacemark)
departureBoatTerminalLocationSelector = mkSelector "departureBoatTerminalLocation"

-- | @Selector@ for @arrivalBoatTerminalLocation@
arrivalBoatTerminalLocationSelector :: Selector '[] (Id CLPlacemark)
arrivalBoatTerminalLocationSelector = mkSelector "arrivalBoatTerminalLocation"

