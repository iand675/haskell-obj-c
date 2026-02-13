{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that contains a flight number that the data detection system matches.
--
-- The DataDetection framework returns a flight number match in a @DDMatchFlightNumber@ object, which contains an airline name and flight number.
--
-- Generated bindings for @DDMatchFlightNumber@.
module ObjC.DataDetection.DDMatchFlightNumber
  ( DDMatchFlightNumber
  , IsDDMatchFlightNumber(..)
  , airline
  , flightNumber
  , airlineSelector
  , flightNumberSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.DataDetection.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The name of an airline.
--
-- ObjC selector: @- airline@
airline :: IsDDMatchFlightNumber ddMatchFlightNumber => ddMatchFlightNumber -> IO (Id NSString)
airline ddMatchFlightNumber =
  sendMessage ddMatchFlightNumber airlineSelector

-- | A string that represents a flight number.
--
-- ObjC selector: @- flightNumber@
flightNumber :: IsDDMatchFlightNumber ddMatchFlightNumber => ddMatchFlightNumber -> IO (Id NSString)
flightNumber ddMatchFlightNumber =
  sendMessage ddMatchFlightNumber flightNumberSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @airline@
airlineSelector :: Selector '[] (Id NSString)
airlineSelector = mkSelector "airline"

-- | @Selector@ for @flightNumber@
flightNumberSelector :: Selector '[] (Id NSString)
flightNumberSelector = mkSelector "flightNumber"

