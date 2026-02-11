{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRestaurantGuest@.
module ObjC.Intents.INRestaurantGuest
  ( INRestaurantGuest
  , IsINRestaurantGuest(..)
  , initWithNameComponents_phoneNumber_emailAddress
  , phoneNumber
  , setPhoneNumber
  , emailAddress
  , setEmailAddress
  , initWithNameComponents_phoneNumber_emailAddressSelector
  , phoneNumberSelector
  , setPhoneNumberSelector
  , emailAddressSelector
  , setEmailAddressSelector


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

-- | @- initWithNameComponents:phoneNumber:emailAddress:@
initWithNameComponents_phoneNumber_emailAddress :: (IsINRestaurantGuest inRestaurantGuest, IsNSPersonNameComponents nameComponents, IsNSString phoneNumber, IsNSString emailAddress) => inRestaurantGuest -> nameComponents -> phoneNumber -> emailAddress -> IO (Id INRestaurantGuest)
initWithNameComponents_phoneNumber_emailAddress inRestaurantGuest  nameComponents phoneNumber emailAddress =
withObjCPtr nameComponents $ \raw_nameComponents ->
  withObjCPtr phoneNumber $ \raw_phoneNumber ->
    withObjCPtr emailAddress $ \raw_emailAddress ->
        sendMsg inRestaurantGuest (mkSelector "initWithNameComponents:phoneNumber:emailAddress:") (retPtr retVoid) [argPtr (castPtr raw_nameComponents :: Ptr ()), argPtr (castPtr raw_phoneNumber :: Ptr ()), argPtr (castPtr raw_emailAddress :: Ptr ())] >>= ownedObject . castPtr

-- | @- phoneNumber@
phoneNumber :: IsINRestaurantGuest inRestaurantGuest => inRestaurantGuest -> IO (Id NSString)
phoneNumber inRestaurantGuest  =
  sendMsg inRestaurantGuest (mkSelector "phoneNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPhoneNumber:@
setPhoneNumber :: (IsINRestaurantGuest inRestaurantGuest, IsNSString value) => inRestaurantGuest -> value -> IO ()
setPhoneNumber inRestaurantGuest  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRestaurantGuest (mkSelector "setPhoneNumber:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- emailAddress@
emailAddress :: IsINRestaurantGuest inRestaurantGuest => inRestaurantGuest -> IO (Id NSString)
emailAddress inRestaurantGuest  =
  sendMsg inRestaurantGuest (mkSelector "emailAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEmailAddress:@
setEmailAddress :: (IsINRestaurantGuest inRestaurantGuest, IsNSString value) => inRestaurantGuest -> value -> IO ()
setEmailAddress inRestaurantGuest  value =
withObjCPtr value $ \raw_value ->
    sendMsg inRestaurantGuest (mkSelector "setEmailAddress:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNameComponents:phoneNumber:emailAddress:@
initWithNameComponents_phoneNumber_emailAddressSelector :: Selector
initWithNameComponents_phoneNumber_emailAddressSelector = mkSelector "initWithNameComponents:phoneNumber:emailAddress:"

-- | @Selector@ for @phoneNumber@
phoneNumberSelector :: Selector
phoneNumberSelector = mkSelector "phoneNumber"

-- | @Selector@ for @setPhoneNumber:@
setPhoneNumberSelector :: Selector
setPhoneNumberSelector = mkSelector "setPhoneNumber:"

-- | @Selector@ for @emailAddress@
emailAddressSelector :: Selector
emailAddressSelector = mkSelector "emailAddress"

-- | @Selector@ for @setEmailAddress:@
setEmailAddressSelector :: Selector
setEmailAddressSelector = mkSelector "setEmailAddress:"

