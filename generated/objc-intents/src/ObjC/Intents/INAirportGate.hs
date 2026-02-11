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
  , initSelector
  , initWithAirport_terminal_gateSelector
  , airportSelector
  , terminalSelector
  , gateSelector


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
init_ :: IsINAirportGate inAirportGate => inAirportGate -> IO (Id INAirportGate)
init_ inAirportGate  =
  sendMsg inAirportGate (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithAirport:terminal:gate:@
initWithAirport_terminal_gate :: (IsINAirportGate inAirportGate, IsINAirport airport, IsNSString terminal, IsNSString gate) => inAirportGate -> airport -> terminal -> gate -> IO (Id INAirportGate)
initWithAirport_terminal_gate inAirportGate  airport terminal gate =
withObjCPtr airport $ \raw_airport ->
  withObjCPtr terminal $ \raw_terminal ->
    withObjCPtr gate $ \raw_gate ->
        sendMsg inAirportGate (mkSelector "initWithAirport:terminal:gate:") (retPtr retVoid) [argPtr (castPtr raw_airport :: Ptr ()), argPtr (castPtr raw_terminal :: Ptr ()), argPtr (castPtr raw_gate :: Ptr ())] >>= ownedObject . castPtr

-- | @- airport@
airport :: IsINAirportGate inAirportGate => inAirportGate -> IO (Id INAirport)
airport inAirportGate  =
  sendMsg inAirportGate (mkSelector "airport") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- terminal@
terminal :: IsINAirportGate inAirportGate => inAirportGate -> IO (Id NSString)
terminal inAirportGate  =
  sendMsg inAirportGate (mkSelector "terminal") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- gate@
gate :: IsINAirportGate inAirportGate => inAirportGate -> IO (Id NSString)
gate inAirportGate  =
  sendMsg inAirportGate (mkSelector "gate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithAirport:terminal:gate:@
initWithAirport_terminal_gateSelector :: Selector
initWithAirport_terminal_gateSelector = mkSelector "initWithAirport:terminal:gate:"

-- | @Selector@ for @airport@
airportSelector :: Selector
airportSelector = mkSelector "airport"

-- | @Selector@ for @terminal@
terminalSelector :: Selector
terminalSelector = mkSelector "terminal"

-- | @Selector@ for @gate@
gateSelector :: Selector
gateSelector = mkSelector "gate"

