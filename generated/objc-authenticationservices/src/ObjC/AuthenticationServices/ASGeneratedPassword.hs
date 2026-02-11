{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASGeneratedPassword@.
module ObjC.AuthenticationServices.ASGeneratedPassword
  ( ASGeneratedPassword
  , IsASGeneratedPassword(..)
  , init_
  , new
  , initWithKind_value
  , kind
  , localizedName
  , value
  , initSelector
  , newSelector
  , initWithKind_valueSelector
  , kindSelector
  , localizedNameSelector
  , valueSelector


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

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsASGeneratedPassword asGeneratedPassword => asGeneratedPassword -> IO (Id ASGeneratedPassword)
init_ asGeneratedPassword  =
  sendMsg asGeneratedPassword (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id ASGeneratedPassword)
new  =
  do
    cls' <- getRequiredClass "ASGeneratedPassword"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithKind:value:@
initWithKind_value :: (IsASGeneratedPassword asGeneratedPassword, IsNSString kind, IsNSString value) => asGeneratedPassword -> kind -> value -> IO (Id ASGeneratedPassword)
initWithKind_value asGeneratedPassword  kind value =
withObjCPtr kind $ \raw_kind ->
  withObjCPtr value $ \raw_value ->
      sendMsg asGeneratedPassword (mkSelector "initWithKind:value:") (retPtr retVoid) [argPtr (castPtr raw_kind :: Ptr ()), argPtr (castPtr raw_value :: Ptr ())] >>= ownedObject . castPtr

-- | The kind of password that this represents.
--
-- ObjC selector: @- kind@
kind :: IsASGeneratedPassword asGeneratedPassword => asGeneratedPassword -> IO (Id NSString)
kind asGeneratedPassword  =
  sendMsg asGeneratedPassword (mkSelector "kind") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The user-visible description of this password, derived from the kind.
--
-- This may be shown to help with selecting the desired password.
--
-- ObjC selector: @- localizedName@
localizedName :: IsASGeneratedPassword asGeneratedPassword => asGeneratedPassword -> IO (Id NSString)
localizedName asGeneratedPassword  =
  sendMsg asGeneratedPassword (mkSelector "localizedName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The value of the password.
--
-- ObjC selector: @- value@
value :: IsASGeneratedPassword asGeneratedPassword => asGeneratedPassword -> IO (Id NSString)
value asGeneratedPassword  =
  sendMsg asGeneratedPassword (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithKind:value:@
initWithKind_valueSelector :: Selector
initWithKind_valueSelector = mkSelector "initWithKind:value:"

-- | @Selector@ for @kind@
kindSelector :: Selector
kindSelector = mkSelector "kind"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

