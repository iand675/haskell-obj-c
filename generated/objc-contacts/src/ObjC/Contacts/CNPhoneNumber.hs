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
  , phoneNumberWithStringValueSelector
  , initWithStringValueSelector
  , initSelector
  , newSelector
  , stringValueSelector


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

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | These will return nil if the stringValue is nil.
--
-- ObjC selector: @+ phoneNumberWithStringValue:@
phoneNumberWithStringValue :: IsNSString stringValue => stringValue -> IO (Id CNPhoneNumber)
phoneNumberWithStringValue stringValue =
  do
    cls' <- getRequiredClass "CNPhoneNumber"
    withObjCPtr stringValue $ \raw_stringValue ->
      sendClassMsg cls' (mkSelector "phoneNumberWithStringValue:") (retPtr retVoid) [argPtr (castPtr raw_stringValue :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithStringValue:@
initWithStringValue :: (IsCNPhoneNumber cnPhoneNumber, IsNSString string) => cnPhoneNumber -> string -> IO (Id CNPhoneNumber)
initWithStringValue cnPhoneNumber  string =
withObjCPtr string $ \raw_string ->
    sendMsg cnPhoneNumber (mkSelector "initWithStringValue:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsCNPhoneNumber cnPhoneNumber => cnPhoneNumber -> IO (Id CNPhoneNumber)
init_ cnPhoneNumber  =
  sendMsg cnPhoneNumber (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CNPhoneNumber)
new  =
  do
    cls' <- getRequiredClass "CNPhoneNumber"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- stringValue@
stringValue :: IsCNPhoneNumber cnPhoneNumber => cnPhoneNumber -> IO (Id NSString)
stringValue cnPhoneNumber  =
  sendMsg cnPhoneNumber (mkSelector "stringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @phoneNumberWithStringValue:@
phoneNumberWithStringValueSelector :: Selector
phoneNumberWithStringValueSelector = mkSelector "phoneNumberWithStringValue:"

-- | @Selector@ for @initWithStringValue:@
initWithStringValueSelector :: Selector
initWithStringValueSelector = mkSelector "initWithStringValue:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector
stringValueSelector = mkSelector "stringValue"

