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
  , initSelector
  , initWithAirline_flightNumber_boardingTime_flightDuration_departureAirportGate_arrivalAirportGateSelector
  , airlineSelector
  , flightNumberSelector
  , boardingTimeSelector
  , flightDurationSelector
  , departureAirportGateSelector
  , arrivalAirportGateSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINFlight inFlight => inFlight -> IO (Id INFlight)
init_ inFlight  =
  sendMsg inFlight (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithAirline:flightNumber:boardingTime:flightDuration:departureAirportGate:arrivalAirportGate:@
initWithAirline_flightNumber_boardingTime_flightDuration_departureAirportGate_arrivalAirportGate :: (IsINFlight inFlight, IsINAirline airline, IsNSString flightNumber, IsINDateComponentsRange boardingTime, IsINDateComponentsRange flightDuration, IsINAirportGate departureAirportGate, IsINAirportGate arrivalAirportGate) => inFlight -> airline -> flightNumber -> boardingTime -> flightDuration -> departureAirportGate -> arrivalAirportGate -> IO (Id INFlight)
initWithAirline_flightNumber_boardingTime_flightDuration_departureAirportGate_arrivalAirportGate inFlight  airline flightNumber boardingTime flightDuration departureAirportGate arrivalAirportGate =
withObjCPtr airline $ \raw_airline ->
  withObjCPtr flightNumber $ \raw_flightNumber ->
    withObjCPtr boardingTime $ \raw_boardingTime ->
      withObjCPtr flightDuration $ \raw_flightDuration ->
        withObjCPtr departureAirportGate $ \raw_departureAirportGate ->
          withObjCPtr arrivalAirportGate $ \raw_arrivalAirportGate ->
              sendMsg inFlight (mkSelector "initWithAirline:flightNumber:boardingTime:flightDuration:departureAirportGate:arrivalAirportGate:") (retPtr retVoid) [argPtr (castPtr raw_airline :: Ptr ()), argPtr (castPtr raw_flightNumber :: Ptr ()), argPtr (castPtr raw_boardingTime :: Ptr ()), argPtr (castPtr raw_flightDuration :: Ptr ()), argPtr (castPtr raw_departureAirportGate :: Ptr ()), argPtr (castPtr raw_arrivalAirportGate :: Ptr ())] >>= ownedObject . castPtr

-- | @- airline@
airline :: IsINFlight inFlight => inFlight -> IO (Id INAirline)
airline inFlight  =
  sendMsg inFlight (mkSelector "airline") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- flightNumber@
flightNumber :: IsINFlight inFlight => inFlight -> IO (Id NSString)
flightNumber inFlight  =
  sendMsg inFlight (mkSelector "flightNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- boardingTime@
boardingTime :: IsINFlight inFlight => inFlight -> IO (Id INDateComponentsRange)
boardingTime inFlight  =
  sendMsg inFlight (mkSelector "boardingTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- flightDuration@
flightDuration :: IsINFlight inFlight => inFlight -> IO (Id INDateComponentsRange)
flightDuration inFlight  =
  sendMsg inFlight (mkSelector "flightDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- departureAirportGate@
departureAirportGate :: IsINFlight inFlight => inFlight -> IO (Id INAirportGate)
departureAirportGate inFlight  =
  sendMsg inFlight (mkSelector "departureAirportGate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- arrivalAirportGate@
arrivalAirportGate :: IsINFlight inFlight => inFlight -> IO (Id INAirportGate)
arrivalAirportGate inFlight  =
  sendMsg inFlight (mkSelector "arrivalAirportGate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithAirline:flightNumber:boardingTime:flightDuration:departureAirportGate:arrivalAirportGate:@
initWithAirline_flightNumber_boardingTime_flightDuration_departureAirportGate_arrivalAirportGateSelector :: Selector
initWithAirline_flightNumber_boardingTime_flightDuration_departureAirportGate_arrivalAirportGateSelector = mkSelector "initWithAirline:flightNumber:boardingTime:flightDuration:departureAirportGate:arrivalAirportGate:"

-- | @Selector@ for @airline@
airlineSelector :: Selector
airlineSelector = mkSelector "airline"

-- | @Selector@ for @flightNumber@
flightNumberSelector :: Selector
flightNumberSelector = mkSelector "flightNumber"

-- | @Selector@ for @boardingTime@
boardingTimeSelector :: Selector
boardingTimeSelector = mkSelector "boardingTime"

-- | @Selector@ for @flightDuration@
flightDurationSelector :: Selector
flightDurationSelector = mkSelector "flightDuration"

-- | @Selector@ for @departureAirportGate@
departureAirportGateSelector :: Selector
departureAirportGateSelector = mkSelector "departureAirportGate"

-- | @Selector@ for @arrivalAirportGate@
arrivalAirportGateSelector :: Selector
arrivalAirportGateSelector = mkSelector "arrivalAirportGate"

