{-# LANGUAGE DataKinds #-}
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
  , emailAddressSelector
  , initWithNameComponents_phoneNumber_emailAddressSelector
  , phoneNumberSelector
  , setEmailAddressSelector
  , setPhoneNumberSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithNameComponents:phoneNumber:emailAddress:@
initWithNameComponents_phoneNumber_emailAddress :: (IsINRestaurantGuest inRestaurantGuest, IsNSPersonNameComponents nameComponents, IsNSString phoneNumber, IsNSString emailAddress) => inRestaurantGuest -> nameComponents -> phoneNumber -> emailAddress -> IO (Id INRestaurantGuest)
initWithNameComponents_phoneNumber_emailAddress inRestaurantGuest nameComponents phoneNumber emailAddress =
  sendOwnedMessage inRestaurantGuest initWithNameComponents_phoneNumber_emailAddressSelector (toNSPersonNameComponents nameComponents) (toNSString phoneNumber) (toNSString emailAddress)

-- | @- phoneNumber@
phoneNumber :: IsINRestaurantGuest inRestaurantGuest => inRestaurantGuest -> IO (Id NSString)
phoneNumber inRestaurantGuest =
  sendMessage inRestaurantGuest phoneNumberSelector

-- | @- setPhoneNumber:@
setPhoneNumber :: (IsINRestaurantGuest inRestaurantGuest, IsNSString value) => inRestaurantGuest -> value -> IO ()
setPhoneNumber inRestaurantGuest value =
  sendMessage inRestaurantGuest setPhoneNumberSelector (toNSString value)

-- | @- emailAddress@
emailAddress :: IsINRestaurantGuest inRestaurantGuest => inRestaurantGuest -> IO (Id NSString)
emailAddress inRestaurantGuest =
  sendMessage inRestaurantGuest emailAddressSelector

-- | @- setEmailAddress:@
setEmailAddress :: (IsINRestaurantGuest inRestaurantGuest, IsNSString value) => inRestaurantGuest -> value -> IO ()
setEmailAddress inRestaurantGuest value =
  sendMessage inRestaurantGuest setEmailAddressSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNameComponents:phoneNumber:emailAddress:@
initWithNameComponents_phoneNumber_emailAddressSelector :: Selector '[Id NSPersonNameComponents, Id NSString, Id NSString] (Id INRestaurantGuest)
initWithNameComponents_phoneNumber_emailAddressSelector = mkSelector "initWithNameComponents:phoneNumber:emailAddress:"

-- | @Selector@ for @phoneNumber@
phoneNumberSelector :: Selector '[] (Id NSString)
phoneNumberSelector = mkSelector "phoneNumber"

-- | @Selector@ for @setPhoneNumber:@
setPhoneNumberSelector :: Selector '[Id NSString] ()
setPhoneNumberSelector = mkSelector "setPhoneNumber:"

-- | @Selector@ for @emailAddress@
emailAddressSelector :: Selector '[] (Id NSString)
emailAddressSelector = mkSelector "emailAddress"

-- | @Selector@ for @setEmailAddress:@
setEmailAddressSelector :: Selector '[Id NSString] ()
setEmailAddressSelector = mkSelector "setEmailAddress:"

