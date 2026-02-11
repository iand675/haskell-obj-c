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
  , initSelector
  , initWithName_iataCode_icaoCodeSelector
  , nameSelector
  , iataCodeSelector
  , icaoCodeSelector


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
init_ :: IsINAirport inAirport => inAirport -> IO (Id INAirport)
init_ inAirport  =
  sendMsg inAirport (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithName:iataCode:icaoCode:@
initWithName_iataCode_icaoCode :: (IsINAirport inAirport, IsNSString name, IsNSString iataCode, IsNSString icaoCode) => inAirport -> name -> iataCode -> icaoCode -> IO (Id INAirport)
initWithName_iataCode_icaoCode inAirport  name iataCode icaoCode =
withObjCPtr name $ \raw_name ->
  withObjCPtr iataCode $ \raw_iataCode ->
    withObjCPtr icaoCode $ \raw_icaoCode ->
        sendMsg inAirport (mkSelector "initWithName:iataCode:icaoCode:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_iataCode :: Ptr ()), argPtr (castPtr raw_icaoCode :: Ptr ())] >>= ownedObject . castPtr

-- | @- name@
name :: IsINAirport inAirport => inAirport -> IO (Id NSString)
name inAirport  =
  sendMsg inAirport (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- iataCode@
iataCode :: IsINAirport inAirport => inAirport -> IO (Id NSString)
iataCode inAirport  =
  sendMsg inAirport (mkSelector "iataCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- icaoCode@
icaoCode :: IsINAirport inAirport => inAirport -> IO (Id NSString)
icaoCode inAirport  =
  sendMsg inAirport (mkSelector "icaoCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithName:iataCode:icaoCode:@
initWithName_iataCode_icaoCodeSelector :: Selector
initWithName_iataCode_icaoCodeSelector = mkSelector "initWithName:iataCode:icaoCode:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @iataCode@
iataCodeSelector :: Selector
iataCodeSelector = mkSelector "iataCode"

-- | @Selector@ for @icaoCode@
icaoCodeSelector :: Selector
icaoCodeSelector = mkSelector "icaoCode"

