{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An immutable value object representing a phone number.
--
-- CNPhoneNumber is thread safe.
--
-- Generated bindings for @CNPhoneNumber@.
module ObjC.Contacts.CNPhoneNumber
  ( CNPhoneNumber
  , IsCNPhoneNumber(..)
  , phoneNumberWithStringValue
  , initWithStringValue
  , init_
  , new
  , stringValue
  , initSelector
  , initWithStringValueSelector
  , newSelector
  , phoneNumberWithStringValueSelector
  , stringValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | These will return nil if the stringValue is nil.
--
-- ObjC selector: @+ phoneNumberWithStringValue:@
phoneNumberWithStringValue :: IsNSString stringValue => stringValue -> IO (Id CNPhoneNumber)
phoneNumberWithStringValue stringValue =
  do
    cls' <- getRequiredClass "CNPhoneNumber"
    sendClassMessage cls' phoneNumberWithStringValueSelector (toNSString stringValue)

-- | @- initWithStringValue:@
initWithStringValue :: (IsCNPhoneNumber cnPhoneNumber, IsNSString string) => cnPhoneNumber -> string -> IO (Id CNPhoneNumber)
initWithStringValue cnPhoneNumber string =
  sendOwnedMessage cnPhoneNumber initWithStringValueSelector (toNSString string)

-- | @- init@
init_ :: IsCNPhoneNumber cnPhoneNumber => cnPhoneNumber -> IO (Id CNPhoneNumber)
init_ cnPhoneNumber =
  sendOwnedMessage cnPhoneNumber initSelector

-- | @+ new@
new :: IO (Id CNPhoneNumber)
new  =
  do
    cls' <- getRequiredClass "CNPhoneNumber"
    sendOwnedClassMessage cls' newSelector

-- | @- stringValue@
stringValue :: IsCNPhoneNumber cnPhoneNumber => cnPhoneNumber -> IO (Id NSString)
stringValue cnPhoneNumber =
  sendMessage cnPhoneNumber stringValueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @phoneNumberWithStringValue:@
phoneNumberWithStringValueSelector :: Selector '[Id NSString] (Id CNPhoneNumber)
phoneNumberWithStringValueSelector = mkSelector "phoneNumberWithStringValue:"

-- | @Selector@ for @initWithStringValue:@
initWithStringValueSelector :: Selector '[Id NSString] (Id CNPhoneNumber)
initWithStringValueSelector = mkSelector "initWithStringValue:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CNPhoneNumber)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CNPhoneNumber)
newSelector = mkSelector "new"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector '[] (Id NSString)
stringValueSelector = mkSelector "stringValue"

