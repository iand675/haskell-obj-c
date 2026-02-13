{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INFlight@.
module ObjC.Intents.INFlight
  ( INFlight
  , IsINFlight(..)
  , init_
  , initWithAirline_flightNumber_boardingTime_flightDuration_departureAirportGate_arrivalAirportGate
  , airline
  , flightNumber
  , boardingTime
  , flightDuration
  , departureAirportGate
  , arrivalAirportGate
  , airlineSelector
  , arrivalAirportGateSelector
  , boardingTimeSelector
  , departureAirportGateSelector
  , flightDurationSelector
  , flightNumberSelector
  , initSelector
  , initWithAirline_flightNumber_boardingTime_flightDuration_departureAirportGate_arrivalAirportGateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINFlight inFlight => inFlight -> IO (Id INFlight)
init_ inFlight =
  sendOwnedMessage inFlight initSelector

-- | @- initWithAirline:flightNumber:boardingTime:flightDuration:departureAirportGate:arrivalAirportGate:@
initWithAirline_flightNumber_boardingTime_flightDuration_departureAirportGate_arrivalAirportGate :: (IsINFlight inFlight, IsINAirline airline, IsNSString flightNumber, IsINDateComponentsRange boardingTime, IsINDateComponentsRange flightDuration, IsINAirportGate departureAirportGate, IsINAirportGate arrivalAirportGate) => inFlight -> airline -> flightNumber -> boardingTime -> flightDuration -> departureAirportGate -> arrivalAirportGate -> IO (Id INFlight)
initWithAirline_flightNumber_boardingTime_flightDuration_departureAirportGate_arrivalAirportGate inFlight airline flightNumber boardingTime flightDuration departureAirportGate arrivalAirportGate =
  sendOwnedMessage inFlight initWithAirline_flightNumber_boardingTime_flightDuration_departureAirportGate_arrivalAirportGateSelector (toINAirline airline) (toNSString flightNumber) (toINDateComponentsRange boardingTime) (toINDateComponentsRange flightDuration) (toINAirportGate departureAirportGate) (toINAirportGate arrivalAirportGate)

-- | @- airline@
airline :: IsINFlight inFlight => inFlight -> IO (Id INAirline)
airline inFlight =
  sendMessage inFlight airlineSelector

-- | @- flightNumber@
flightNumber :: IsINFlight inFlight => inFlight -> IO (Id NSString)
flightNumber inFlight =
  sendMessage inFlight flightNumberSelector

-- | @- boardingTime@
boardingTime :: IsINFlight inFlight => inFlight -> IO (Id INDateComponentsRange)
boardingTime inFlight =
  sendMessage inFlight boardingTimeSelector

-- | @- flightDuration@
flightDuration :: IsINFlight inFlight => inFlight -> IO (Id INDateComponentsRange)
flightDuration inFlight =
  sendMessage inFlight flightDurationSelector

-- | @- departureAirportGate@
departureAirportGate :: IsINFlight inFlight => inFlight -> IO (Id INAirportGate)
departureAirportGate inFlight =
  sendMessage inFlight departureAirportGateSelector

-- | @- arrivalAirportGate@
arrivalAirportGate :: IsINFlight inFlight => inFlight -> IO (Id INAirportGate)
arrivalAirportGate inFlight =
  sendMessage inFlight arrivalAirportGateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INFlight)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithAirline:flightNumber:boardingTime:flightDuration:departureAirportGate:arrivalAirportGate:@
initWithAirline_flightNumber_boardingTime_flightDuration_departureAirportGate_arrivalAirportGateSelector :: Selector '[Id INAirline, Id NSString, Id INDateComponentsRange, Id INDateComponentsRange, Id INAirportGate, Id INAirportGate] (Id INFlight)
initWithAirline_flightNumber_boardingTime_flightDuration_departureAirportGate_arrivalAirportGateSelector = mkSelector "initWithAirline:flightNumber:boardingTime:flightDuration:departureAirportGate:arrivalAirportGate:"

-- | @Selector@ for @airline@
airlineSelector :: Selector '[] (Id INAirline)
airlineSelector = mkSelector "airline"

-- | @Selector@ for @flightNumber@
flightNumberSelector :: Selector '[] (Id NSString)
flightNumberSelector = mkSelector "flightNumber"

-- | @Selector@ for @boardingTime@
boardingTimeSelector :: Selector '[] (Id INDateComponentsRange)
boardingTimeSelector = mkSelector "boardingTime"

-- | @Selector@ for @flightDuration@
flightDurationSelector :: Selector '[] (Id INDateComponentsRange)
flightDurationSelector = mkSelector "flightDuration"

-- | @Selector@ for @departureAirportGate@
departureAirportGateSelector :: Selector '[] (Id INAirportGate)
departureAirportGateSelector = mkSelector "departureAirportGate"

-- | @Selector@ for @arrivalAirportGate@
arrivalAirportGateSelector :: Selector '[] (Id INAirportGate)
arrivalAirportGateSelector = mkSelector "arrivalAirportGate"

