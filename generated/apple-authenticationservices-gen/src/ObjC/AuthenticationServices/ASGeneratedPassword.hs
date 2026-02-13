{-# LANGUAGE DataKinds #-}
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
  , initWithKind_valueSelector
  , kindSelector
  , localizedNameSelector
  , newSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AuthenticationServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsASGeneratedPassword asGeneratedPassword => asGeneratedPassword -> IO (Id ASGeneratedPassword)
init_ asGeneratedPassword =
  sendOwnedMessage asGeneratedPassword initSelector

-- | @+ new@
new :: IO (Id ASGeneratedPassword)
new  =
  do
    cls' <- getRequiredClass "ASGeneratedPassword"
    sendOwnedClassMessage cls' newSelector

-- | @- initWithKind:value:@
initWithKind_value :: (IsASGeneratedPassword asGeneratedPassword, IsNSString kind, IsNSString value) => asGeneratedPassword -> kind -> value -> IO (Id ASGeneratedPassword)
initWithKind_value asGeneratedPassword kind value =
  sendOwnedMessage asGeneratedPassword initWithKind_valueSelector (toNSString kind) (toNSString value)

-- | The kind of password that this represents.
--
-- ObjC selector: @- kind@
kind :: IsASGeneratedPassword asGeneratedPassword => asGeneratedPassword -> IO (Id NSString)
kind asGeneratedPassword =
  sendMessage asGeneratedPassword kindSelector

-- | The user-visible description of this password, derived from the kind.
--
-- This may be shown to help with selecting the desired password.
--
-- ObjC selector: @- localizedName@
localizedName :: IsASGeneratedPassword asGeneratedPassword => asGeneratedPassword -> IO (Id NSString)
localizedName asGeneratedPassword =
  sendMessage asGeneratedPassword localizedNameSelector

-- | The value of the password.
--
-- ObjC selector: @- value@
value :: IsASGeneratedPassword asGeneratedPassword => asGeneratedPassword -> IO (Id NSString)
value asGeneratedPassword =
  sendMessage asGeneratedPassword valueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ASGeneratedPassword)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ASGeneratedPassword)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithKind:value:@
initWithKind_valueSelector :: Selector '[Id NSString, Id NSString] (Id ASGeneratedPassword)
initWithKind_valueSelector = mkSelector "initWithKind:value:"

-- | @Selector@ for @kind@
kindSelector :: Selector '[] (Id NSString)
kindSelector = mkSelector "kind"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector '[] (Id NSString)
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSString)
valueSelector = mkSelector "value"

