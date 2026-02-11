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
  , nameFieldFirstNameOptionalSelector
  , setNameFieldFirstNameOptionalSelector
  , nameFieldLastNameOptionalSelector
  , setNameFieldLastNameOptionalSelector
  , nameFieldShouldBeDisplayedSelector
  , setNameFieldShouldBeDisplayedSelector
  , emailAddressFieldShouldBeDisplayedSelector
  , setEmailAddressFieldShouldBeDisplayedSelector
  , phoneNumberFieldShouldBeDisplayedSelector
  , setPhoneNumberFieldShouldBeDisplayedSelector
  , nameEditableSelector
  , setNameEditableSelector
  , emailAddressEditableSelector
  , setEmailAddressEditableSelector
  , phoneNumberEditableSelector
  , setPhoneNumberEditableSelector


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

-- | @- nameFieldFirstNameOptional@
nameFieldFirstNameOptional :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> IO Bool
nameFieldFirstNameOptional inRestaurantGuestDisplayPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inRestaurantGuestDisplayPreferences (mkSelector "nameFieldFirstNameOptional") retCULong []

-- | @- setNameFieldFirstNameOptional:@
setNameFieldFirstNameOptional :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> Bool -> IO ()
setNameFieldFirstNameOptional inRestaurantGuestDisplayPreferences  value =
  sendMsg inRestaurantGuestDisplayPreferences (mkSelector "setNameFieldFirstNameOptional:") retVoid [argCULong (if value then 1 else 0)]

-- | @- nameFieldLastNameOptional@
nameFieldLastNameOptional :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> IO Bool
nameFieldLastNameOptional inRestaurantGuestDisplayPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inRestaurantGuestDisplayPreferences (mkSelector "nameFieldLastNameOptional") retCULong []

-- | @- setNameFieldLastNameOptional:@
setNameFieldLastNameOptional :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> Bool -> IO ()
setNameFieldLastNameOptional inRestaurantGuestDisplayPreferences  value =
  sendMsg inRestaurantGuestDisplayPreferences (mkSelector "setNameFieldLastNameOptional:") retVoid [argCULong (if value then 1 else 0)]

-- | @- nameFieldShouldBeDisplayed@
nameFieldShouldBeDisplayed :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> IO Bool
nameFieldShouldBeDisplayed inRestaurantGuestDisplayPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inRestaurantGuestDisplayPreferences (mkSelector "nameFieldShouldBeDisplayed") retCULong []

-- | @- setNameFieldShouldBeDisplayed:@
setNameFieldShouldBeDisplayed :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> Bool -> IO ()
setNameFieldShouldBeDisplayed inRestaurantGuestDisplayPreferences  value =
  sendMsg inRestaurantGuestDisplayPreferences (mkSelector "setNameFieldShouldBeDisplayed:") retVoid [argCULong (if value then 1 else 0)]

-- | @- emailAddressFieldShouldBeDisplayed@
emailAddressFieldShouldBeDisplayed :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> IO Bool
emailAddressFieldShouldBeDisplayed inRestaurantGuestDisplayPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inRestaurantGuestDisplayPreferences (mkSelector "emailAddressFieldShouldBeDisplayed") retCULong []

-- | @- setEmailAddressFieldShouldBeDisplayed:@
setEmailAddressFieldShouldBeDisplayed :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> Bool -> IO ()
setEmailAddressFieldShouldBeDisplayed inRestaurantGuestDisplayPreferences  value =
  sendMsg inRestaurantGuestDisplayPreferences (mkSelector "setEmailAddressFieldShouldBeDisplayed:") retVoid [argCULong (if value then 1 else 0)]

-- | @- phoneNumberFieldShouldBeDisplayed@
phoneNumberFieldShouldBeDisplayed :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> IO Bool
phoneNumberFieldShouldBeDisplayed inRestaurantGuestDisplayPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inRestaurantGuestDisplayPreferences (mkSelector "phoneNumberFieldShouldBeDisplayed") retCULong []

-- | @- setPhoneNumberFieldShouldBeDisplayed:@
setPhoneNumberFieldShouldBeDisplayed :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> Bool -> IO ()
setPhoneNumberFieldShouldBeDisplayed inRestaurantGuestDisplayPreferences  value =
  sendMsg inRestaurantGuestDisplayPreferences (mkSelector "setPhoneNumberFieldShouldBeDisplayed:") retVoid [argCULong (if value then 1 else 0)]

-- | @- nameEditable@
nameEditable :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> IO Bool
nameEditable inRestaurantGuestDisplayPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inRestaurantGuestDisplayPreferences (mkSelector "nameEditable") retCULong []

-- | @- setNameEditable:@
setNameEditable :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> Bool -> IO ()
setNameEditable inRestaurantGuestDisplayPreferences  value =
  sendMsg inRestaurantGuestDisplayPreferences (mkSelector "setNameEditable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- emailAddressEditable@
emailAddressEditable :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> IO Bool
emailAddressEditable inRestaurantGuestDisplayPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inRestaurantGuestDisplayPreferences (mkSelector "emailAddressEditable") retCULong []

-- | @- setEmailAddressEditable:@
setEmailAddressEditable :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> Bool -> IO ()
setEmailAddressEditable inRestaurantGuestDisplayPreferences  value =
  sendMsg inRestaurantGuestDisplayPreferences (mkSelector "setEmailAddressEditable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- phoneNumberEditable@
phoneNumberEditable :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> IO Bool
phoneNumberEditable inRestaurantGuestDisplayPreferences  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg inRestaurantGuestDisplayPreferences (mkSelector "phoneNumberEditable") retCULong []

-- | @- setPhoneNumberEditable:@
setPhoneNumberEditable :: IsINRestaurantGuestDisplayPreferences inRestaurantGuestDisplayPreferences => inRestaurantGuestDisplayPreferences -> Bool -> IO ()
setPhoneNumberEditable inRestaurantGuestDisplayPreferences  value =
  sendMsg inRestaurantGuestDisplayPreferences (mkSelector "setPhoneNumberEditable:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nameFieldFirstNameOptional@
nameFieldFirstNameOptionalSelector :: Selector
nameFieldFirstNameOptionalSelector = mkSelector "nameFieldFirstNameOptional"

-- | @Selector@ for @setNameFieldFirstNameOptional:@
setNameFieldFirstNameOptionalSelector :: Selector
setNameFieldFirstNameOptionalSelector = mkSelector "setNameFieldFirstNameOptional:"

-- | @Selector@ for @nameFieldLastNameOptional@
nameFieldLastNameOptionalSelector :: Selector
nameFieldLastNameOptionalSelector = mkSelector "nameFieldLastNameOptional"

-- | @Selector@ for @setNameFieldLastNameOptional:@
setNameFieldLastNameOptionalSelector :: Selector
setNameFieldLastNameOptionalSelector = mkSelector "setNameFieldLastNameOptional:"

-- | @Selector@ for @nameFieldShouldBeDisplayed@
nameFieldShouldBeDisplayedSelector :: Selector
nameFieldShouldBeDisplayedSelector = mkSelector "nameFieldShouldBeDisplayed"

-- | @Selector@ for @setNameFieldShouldBeDisplayed:@
setNameFieldShouldBeDisplayedSelector :: Selector
setNameFieldShouldBeDisplayedSelector = mkSelector "setNameFieldShouldBeDisplayed:"

-- | @Selector@ for @emailAddressFieldShouldBeDisplayed@
emailAddressFieldShouldBeDisplayedSelector :: Selector
emailAddressFieldShouldBeDisplayedSelector = mkSelector "emailAddressFieldShouldBeDisplayed"

-- | @Selector@ for @setEmailAddressFieldShouldBeDisplayed:@
setEmailAddressFieldShouldBeDisplayedSelector :: Selector
setEmailAddressFieldShouldBeDisplayedSelector = mkSelector "setEmailAddressFieldShouldBeDisplayed:"

-- | @Selector@ for @phoneNumberFieldShouldBeDisplayed@
phoneNumberFieldShouldBeDisplayedSelector :: Selector
phoneNumberFieldShouldBeDisplayedSelector = mkSelector "phoneNumberFieldShouldBeDisplayed"

-- | @Selector@ for @setPhoneNumberFieldShouldBeDisplayed:@
setPhoneNumberFieldShouldBeDisplayedSelector :: Selector
setPhoneNumberFieldShouldBeDisplayedSelector = mkSelector "setPhoneNumberFieldShouldBeDisplayed:"

-- | @Selector@ for @nameEditable@
nameEditableSelector :: Selector
nameEditableSelector = mkSelector "nameEditable"

-- | @Selector@ for @setNameEditable:@
setNameEditableSelector :: Selector
setNameEditableSelector = mkSelector "setNameEditable:"

-- | @Selector@ for @emailAddressEditable@
emailAddressEditableSelector :: Selector
emailAddressEditableSelector = mkSelector "emailAddressEditable"

-- | @Selector@ for @setEmailAddressEditable:@
setEmailAddressEditableSelector :: Selector
setEmailAddressEditableSelector = mkSelector "setEmailAddressEditable:"

-- | @Selector@ for @phoneNumberEditable@
phoneNumberEditableSelector :: Selector
phoneNumberEditableSelector = mkSelector "phoneNumberEditable"

-- | @Selector@ for @setPhoneNumberEditable:@
setPhoneNumberEditableSelector :: Selector
setPhoneNumberEditableSelector = mkSelector "setPhoneNumberEditable:"

