{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INAirport@.
module ObjC.Intents.INAirport
  ( INAirport
  , IsINAirport(..)
  , init_
  , initWithName_iataCode_icaoCode
  , name
  , iataCode
  , icaoCode
  , iataCodeSelector
  , icaoCodeSelector
  , initSelector
  , initWithName_iataCode_icaoCodeSelector
  , nameSelector


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
init_ :: IsINAirport inAirport => inAirport -> IO (Id INAirport)
init_ inAirport =
  sendOwnedMessage inAirport initSelector

-- | @- initWithName:iataCode:icaoCode:@
initWithName_iataCode_icaoCode :: (IsINAirport inAirport, IsNSString name, IsNSString iataCode, IsNSString icaoCode) => inAirport -> name -> iataCode -> icaoCode -> IO (Id INAirport)
initWithName_iataCode_icaoCode inAirport name iataCode icaoCode =
  sendOwnedMessage inAirport initWithName_iataCode_icaoCodeSelector (toNSString name) (toNSString iataCode) (toNSString icaoCode)

-- | @- name@
name :: IsINAirport inAirport => inAirport -> IO (Id NSString)
name inAirport =
  sendMessage inAirport nameSelector

-- | @- iataCode@
iataCode :: IsINAirport inAirport => inAirport -> IO (Id NSString)
iataCode inAirport =
  sendMessage inAirport iataCodeSelector

-- | @- icaoCode@
icaoCode :: IsINAirport inAirport => inAirport -> IO (Id NSString)
icaoCode inAirport =
  sendMessage inAirport icaoCodeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INAirport)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithName:iataCode:icaoCode:@
initWithName_iataCode_icaoCodeSelector :: Selector '[Id NSString, Id NSString, Id NSString] (Id INAirport)
initWithName_iataCode_icaoCodeSelector = mkSelector "initWithName:iataCode:icaoCode:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @iataCode@
iataCodeSelector :: Selector '[] (Id NSString)
iataCodeSelector = mkSelector "iataCode"

-- | @Selector@ for @icaoCode@
icaoCodeSelector :: Selector '[] (Id NSString)
icaoCodeSelector = mkSelector "icaoCode"

