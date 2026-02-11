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
init_ :: IsINAirline inAirline => inAirline -> IO (Id INAirline)
init_ inAirline  =
  sendMsg inAirline (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithName:iataCode:icaoCode:@
initWithName_iataCode_icaoCode :: (IsINAirline inAirline, IsNSString name, IsNSString iataCode, IsNSString icaoCode) => inAirline -> name -> iataCode -> icaoCode -> IO (Id INAirline)
initWithName_iataCode_icaoCode inAirline  name iataCode icaoCode =
withObjCPtr name $ \raw_name ->
  withObjCPtr iataCode $ \raw_iataCode ->
    withObjCPtr icaoCode $ \raw_icaoCode ->
        sendMsg inAirline (mkSelector "initWithName:iataCode:icaoCode:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_iataCode :: Ptr ()), argPtr (castPtr raw_icaoCode :: Ptr ())] >>= ownedObject . castPtr

-- | @- name@
name :: IsINAirline inAirline => inAirline -> IO (Id NSString)
name inAirline  =
  sendMsg inAirline (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- iataCode@
iataCode :: IsINAirline inAirline => inAirline -> IO (Id NSString)
iataCode inAirline  =
  sendMsg inAirline (mkSelector "iataCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- icaoCode@
icaoCode :: IsINAirline inAirline => inAirline -> IO (Id NSString)
icaoCode inAirline  =
  sendMsg inAirline (mkSelector "icaoCode") (retPtr retVoid) [] >>= retainedObject . castPtr

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

