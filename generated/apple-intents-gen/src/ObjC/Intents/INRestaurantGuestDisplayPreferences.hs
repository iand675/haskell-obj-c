{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRestaurantGuestDisplayPreferences@.
module ObjC.Intents.INRestaurantGuestDisplayPreferences
  ( INRestaurantGuestDisplayPreferences
  , IsINRestaurantGuestDisplayPreferences(..)
  , nameFieldFirstNameOptional
  , setNameFieldFirstNameOptional
  , nameFieldLastNameOptional
  , setNameFieldLastNameOptional
  , nameFieldShouldBeDisplayed
  , setNameFieldShouldBeDisplayed
  , emailAddressFieldShouldBeDisplayed
  , setEmailAddressFieldShouldBeDisplayed
  , phoneNumberFieldShouldBeDisplayed
  , setPhoneNumberFieldShouldBeDisplayed
  , nameEditable
  , setNameEditable
  , emailAddressEditable
  , setEmailAddressEditable
  , phoneNumberEditable
  , setPhoneNumberEditable
  , emailAddressEditableSelector
  , emailAddressFieldShouldBeDisplayedSelector
  , nameEditableSelector
  , nameFieldFirstNameOptionalSelector
  , nameFieldLastNameOptionalSelector
  , nameFieldShouldBeDisplayedSelector
  , phoneNumberEditableSelector
  , phoneNumberFieldShouldBeDisplayedSelector
  , setEmailAddressEditableSelector
  , setEmailAddressFieldShouldBeDisplayedSelector
  , setNameEditableSelector
  , setNameFieldFirstNameOptionalSelector
  , setNameFieldLastNameOptionalSelector
  , setNameFieldShouldBeDisplayedSelector
  , setPhoneNumberEditableSelector
  , setPhoneNumberFieldShouldBeDisplayedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- nameFieldFirstNameOptional@
nameFieldFirstNameOptional :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> IO Bool
nameFieldFirstNameOptional inRestaurantGuestDisplayPreferences =
  sendMessage inRestaurantGuestDisplayPreferences nameFieldFirstNameOptionalSelector

-- | @- setNameFieldFirstNameOptional:@
setNameFieldFirstNameOptional :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> Bool -> IO ()
setNameFieldFirstNameOptional inRestaurantGuestDisplayPreferences value =
  sendMessage inRestaurantGuestDisplayPreferences setNameFieldFirstNameOptionalSelector value

-- | @- nameFieldLastNameOptional@
nameFieldLastNameOptional :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> IO Bool
nameFieldLastNameOptional inRestaurantGuestDisplayPreferences =
  sendMessage inRestaurantGuestDisplayPreferences nameFieldLastNameOptionalSelector

-- | @- setNameFieldLastNameOptional:@
setNameFieldLastNameOptional :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> Bool -> IO ()
setNameFieldLastNameOptional inRestaurantGuestDisplayPreferences value =
  sendMessage inRestaurantGuestDisplayPreferences setNameFieldLastNameOptionalSelector value

-- | @- nameFieldShouldBeDisplayed@
nameFieldShouldBeDisplayed :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> IO Bool
nameFieldShouldBeDisplayed inRestaurantGuestDisplayPreferences =
  sendMessage inRestaurantGuestDisplayPreferences nameFieldShouldBeDisplayedSelector

-- | @- setNameFieldShouldBeDisplayed:@
setNameFieldShouldBeDisplayed :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> Bool -> IO ()
setNameFieldShouldBeDisplayed inRestaurantGuestDisplayPreferences value =
  sendMessage inRestaurantGuestDisplayPreferences setNameFieldShouldBeDisplayedSelector value

-- | @- emailAddressFieldShouldBeDisplayed@
emailAddressFieldShouldBeDisplayed :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> IO Bool
emailAddressFieldShouldBeDisplayed inRestaurantGuestDisplayPreferences =
  sendMessage inRestaurantGuestDisplayPreferences emailAddressFieldShouldBeDisplayedSelector

-- | @- setEmailAddressFieldShouldBeDisplayed:@
setEmailAddressFieldShouldBeDisplayed :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> Bool -> IO ()
setEmailAddressFieldShouldBeDisplayed inRestaurantGuestDisplayPreferences value =
  sendMessage inRestaurantGuestDisplayPreferences setEmailAddressFieldShouldBeDisplayedSelector value

-- | @- phoneNumberFieldShouldBeDisplayed@
phoneNumberFieldShouldBeDisplayed :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> IO Bool
phoneNumberFieldShouldBeDisplayed inRestaurantGuestDisplayPreferences =
  sendMessage inRestaurantGuestDisplayPreferences phoneNumberFieldShouldBeDisplayedSelector

-- | @- setPhoneNumberFieldShouldBeDisplayed:@
setPhoneNumberFieldShouldBeDisplayed :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> Bool -> IO ()
setPhoneNumberFieldShouldBeDisplayed inRestaurantGuestDisplayPreferences value =
  sendMessage inRestaurantGuestDisplayPreferences setPhoneNumberFieldShouldBeDisplayedSelector value

-- | @- nameEditable@
nameEditable :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> IO Bool
nameEditable inRestaurantGuestDisplayPreferences =
  sendMessage inRestaurantGuestDisplayPreferences nameEditableSelector

-- | @- setNameEditable:@
setNameEditable :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> Bool -> IO ()
setNameEditable inRestaurantGuestDisplayPreferences value =
  sendMessage inRestaurantGuestDisplayPreferences setNameEditableSelector value

-- | @- emailAddressEditable@
emailAddressEditable :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> IO Bool
emailAddressEditable inRestaurantGuestDisplayPreferences =
  sendMessage inRestaurantGuestDisplayPreferences emailAddressEditableSelector

-- | @- setEmailAddressEditable:@
setEmailAddressEditable :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> Bool -> IO ()
setEmailAddressEditable inRestaurantGuestDisplayPreferences value =
  sendMessage inRestaurantGuestDisplayPreferences setEmailAddressEditableSelector value

-- | @- phoneNumberEditable@
phoneNumberEditable :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> IO Bool
phoneNumberEditable inRestaurantGuestDisplayPreferences =
  sendMessage inRestaurantGuestDisplayPreferences phoneNumberEditableSelector

-- | @- setPhoneNumberEditable:@
setPhoneNumberEditable :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> Bool -> IO ()
setPhoneNumberEditable inRestaurantGuestDisplayPreferences value =
  sendMessage inRestaurantGuestDisplayPreferences setPhoneNumberEditableSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nameFieldFirstNameOptional@
nameFieldFirstNameOptionalSelector :: Selector '[] Bool
nameFieldFirstNameOptionalSelector = mkSelector "nameFieldFirstNameOptional"

-- | @Selector@ for @setNameFieldFirstNameOptional:@
setNameFieldFirstNameOptionalSelector :: Selector '[Bool] ()
setNameFieldFirstNameOptionalSelector = mkSelector "setNameFieldFirstNameOptional:"

-- | @Selector@ for @nameFieldLastNameOptional@
nameFieldLastNameOptionalSelector :: Selector '[] Bool
nameFieldLastNameOptionalSelector = mkSelector "nameFieldLastNameOptional"

-- | @Selector@ for @setNameFieldLastNameOptional:@
setNameFieldLastNameOptionalSelector :: Selector '[Bool] ()
setNameFieldLastNameOptionalSelector = mkSelector "setNameFieldLastNameOptional:"

-- | @Selector@ for @nameFieldShouldBeDisplayed@
nameFieldShouldBeDisplayedSelector :: Selector '[] Bool
nameFieldShouldBeDisplayedSelector = mkSelector "nameFieldShouldBeDisplayed"

-- | @Selector@ for @setNameFieldShouldBeDisplayed:@
setNameFieldShouldBeDisplayedSelector :: Selector '[Bool] ()
setNameFieldShouldBeDisplayedSelector = mkSelector "setNameFieldShouldBeDisplayed:"

-- | @Selector@ for @emailAddressFieldShouldBeDisplayed@
emailAddressFieldShouldBeDisplayedSelector :: Selector '[] Bool
emailAddressFieldShouldBeDisplayedSelector = mkSelector "emailAddressFieldShouldBeDisplayed"

-- | @Selector@ for @setEmailAddressFieldShouldBeDisplayed:@
setEmailAddressFieldShouldBeDisplayedSelector :: Selector '[Bool] ()
setEmailAddressFieldShouldBeDisplayedSelector = mkSelector "setEmailAddressFieldShouldBeDisplayed:"

-- | @Selector@ for @phoneNumberFieldShouldBeDisplayed@
phoneNumberFieldShouldBeDisplayedSelector :: Selector '[] Bool
phoneNumberFieldShouldBeDisplayedSelector = mkSelector "phoneNumberFieldShouldBeDisplayed"

-- | @Selector@ for @setPhoneNumberFieldShouldBeDisplayed:@
setPhoneNumberFieldShouldBeDisplayedSelector :: Selector '[Bool] ()
setPhoneNumberFieldShouldBeDisplayedSelector = mkSelector "setPhoneNumberFieldShouldBeDisplayed:"

-- | @Selector@ for @nameEditable@
nameEditableSelector :: Selector '[] Bool
nameEditableSelector = mkSelector "nameEditable"

-- | @Selector@ for @setNameEditable:@
setNameEditableSelector :: Selector '[Bool] ()
setNameEditableSelector = mkSelector "setNameEditable:"

-- | @Selector@ for @emailAddressEditable@
emailAddressEditableSelector :: Selector '[] Bool
emailAddressEditableSelector = mkSelector "emailAddressEditable"

-- | @Selector@ for @setEmailAddressEditable:@
setEmailAddressEditableSelector :: Selector '[Bool] ()
setEmailAddressEditableSelector = mkSelector "setEmailAddressEditable:"

-- | @Selector@ for @phoneNumberEditable@
phoneNumberEditableSelector :: Selector '[] Bool
phoneNumberEditableSelector = mkSelector "phoneNumberEditable"

-- | @Selector@ for @setPhoneNumberEditable:@
setPhoneNumberEditableSelector :: Selector '[Bool] ()
setPhoneNumberEditableSelector = mkSelector "setPhoneNumberEditable:"

