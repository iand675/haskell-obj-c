{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INAirportGate@.
module ObjC.Intents.INAirportGate
  ( INAirportGate
  , IsINAirportGate(..)
  , init_
  , initWithAirport_terminal_gate
  , airport
  , terminal
  , gate
  , airportSelector
  , gateSelector
  , initSelector
  , initWithAirport_terminal_gateSelector
  , terminalSelector


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
init_ :: IsINAirportGate inAirportGate => inAirportGate -> IO (Id INAirportGate)
init_ inAirportGate =
  sendOwnedMessage inAirportGate initSelector

-- | @- initWithAirport:terminal:gate:@
initWithAirport_terminal_gate :: (IsINAirportGate inAirportGate, IsINAirport airport, IsNSString terminal, IsNSString gate) => inAirportGate -> airport -> terminal -> gate -> IO (Id INAirportGate)
initWithAirport_terminal_gate inAirportGate airport terminal gate =
  sendOwnedMessage inAirportGate initWithAirport_terminal_gateSelector (toINAirport airport) (toNSString terminal) (toNSString gate)

-- | @- airport@
airport :: IsINAirportGate inAirportGate => inAirportGate -> IO (Id INAirport)
airport inAirportGate =
  sendMessage inAirportGate airportSelector

-- | @- terminal@
terminal :: IsINAirportGate inAirportGate => inAirportGate -> IO (Id NSString)
terminal inAirportGate =
  sendMessage inAirportGate terminalSelector

-- | @- gate@
gate :: IsINAirportGate inAirportGate => inAirportGate -> IO (Id NSString)
gate inAirportGate =
  sendMessage inAirportGate gateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INAirportGate)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithAirport:terminal:gate:@
initWithAirport_terminal_gateSelector :: Selector '[Id INAirport, Id NSString, Id NSString] (Id INAirportGate)
initWithAirport_terminal_gateSelector = mkSelector "initWithAirport:terminal:gate:"

-- | @Selector@ for @airport@
airportSelector :: Selector '[] (Id INAirport)
airportSelector = mkSelector "airport"

-- | @Selector@ for @terminal@
terminalSelector :: Selector '[] (Id NSString)
terminalSelector = mkSelector "terminal"

-- | @Selector@ for @gate@
gateSelector :: Selector '[] (Id NSString)
gateSelector = mkSelector "gate"

