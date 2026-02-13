{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INAirline@.
module ObjC.Intents.INAirline
  ( INAirline
  , IsINAirline(..)
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
init_ :: IsINAirline inAirline => inAirline -> IO (Id INAirline)
init_ inAirline =
  sendOwnedMessage inAirline initSelector

-- | @- initWithName:iataCode:icaoCode:@
initWithName_iataCode_icaoCode :: (IsINAirline inAirline, IsNSString name, IsNSString iataCode, IsNSString icaoCode) => inAirline -> name -> iataCode -> icaoCode -> IO (Id INAirline)
initWithName_iataCode_icaoCode inAirline name iataCode icaoCode =
  sendOwnedMessage inAirline initWithName_iataCode_icaoCodeSelector (toNSString name) (toNSString iataCode) (toNSString icaoCode)

-- | @- name@
name :: IsINAirline inAirline => inAirline -> IO (Id NSString)
name inAirline =
  sendMessage inAirline nameSelector

-- | @- iataCode@
iataCode :: IsINAirline inAirline => inAirline -> IO (Id NSString)
iataCode inAirline =
  sendMessage inAirline iataCodeSelector

-- | @- icaoCode@
icaoCode :: IsINAirline inAirline => inAirline -> IO (Id NSString)
icaoCode inAirline =
  sendMessage inAirline icaoCodeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INAirline)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithName:iataCode:icaoCode:@
initWithName_iataCode_icaoCodeSelector :: Selector '[Id NSString, Id NSString, Id NSString] (Id INAirline)
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

