{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INGetAvailableRestaurantReservationBookingsIntentResponse@.
module ObjC.Intents.INGetAvailableRestaurantReservationBookingsIntentResponse
  ( INGetAvailableRestaurantReservationBookingsIntentResponse
  , IsINGetAvailableRestaurantReservationBookingsIntentResponse(..)
  , initWithAvailableBookings_code_userActivity
  , code
  , localizedRestaurantDescriptionText
  , setLocalizedRestaurantDescriptionText
  , localizedBookingAdvisementText
  , setLocalizedBookingAdvisementText
  , termsAndConditions
  , setTermsAndConditions
  , availableBookings
  , availableBookingsSelector
  , codeSelector
  , initWithAvailableBookings_code_userActivitySelector
  , localizedBookingAdvisementTextSelector
  , localizedRestaurantDescriptionTextSelector
  , setLocalizedBookingAdvisementTextSelector
  , setLocalizedRestaurantDescriptionTextSelector
  , setTermsAndConditionsSelector
  , termsAndConditionsSelector

  -- * Enum types
  , INGetAvailableRestaurantReservationBookingsIntentCode(INGetAvailableRestaurantReservationBookingsIntentCode)
  , pattern INGetAvailableRestaurantReservationBookingsIntentCodeSuccess
  , pattern INGetAvailableRestaurantReservationBookingsIntentCodeFailure
  , pattern INGetAvailableRestaurantReservationBookingsIntentCodeFailureRequestUnsatisfiable
  , pattern INGetAvailableRestaurantReservationBookingsIntentCodeFailureRequestUnspecified

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithAvailableBookings:code:userActivity:@
initWithAvailableBookings_code_userActivity :: (IsINGetAvailableRestaurantReservationBookingsIntentResponse inGetAvailableRestaurantReservationBookingsIntentResponse, IsNSArray availableBookings, IsNSUserActivity userActivity) => inGetAvailableRestaurantReservationBookingsIntentResponse -> availableBookings -> INGetAvailableRestaurantReservationBookingsIntentCode -> userActivity -> IO (Id INGetAvailableRestaurantReservationBookingsIntentResponse)
initWithAvailableBookings_code_userActivity inGetAvailableRestaurantReservationBookingsIntentResponse availableBookings code userActivity =
  sendOwnedMessage inGetAvailableRestaurantReservationBookingsIntentResponse initWithAvailableBookings_code_userActivitySelector (toNSArray availableBookings) code (toNSUserActivity userActivity)

-- | @- code@
code :: IsINGetAvailableRestaurantReservationBookingsIntentResponse inGetAvailableRestaurantReservationBookingsIntentResponse => inGetAvailableRestaurantReservationBookingsIntentResponse -> IO INGetAvailableRestaurantReservationBookingsIntentCode
code inGetAvailableRestaurantReservationBookingsIntentResponse =
  sendMessage inGetAvailableRestaurantReservationBookingsIntentResponse codeSelector

-- | @- localizedRestaurantDescriptionText@
localizedRestaurantDescriptionText :: IsINGetAvailableRestaurantReservationBookingsIntentResponse inGetAvailableRestaurantReservationBookingsIntentResponse => inGetAvailableRestaurantReservationBookingsIntentResponse -> IO (Id NSString)
localizedRestaurantDescriptionText inGetAvailableRestaurantReservationBookingsIntentResponse =
  sendMessage inGetAvailableRestaurantReservationBookingsIntentResponse localizedRestaurantDescriptionTextSelector

-- | @- setLocalizedRestaurantDescriptionText:@
setLocalizedRestaurantDescriptionText :: (IsINGetAvailableRestaurantReservationBookingsIntentResponse inGetAvailableRestaurantReservationBookingsIntentResponse, IsNSString value) => inGetAvailableRestaurantReservationBookingsIntentResponse -> value -> IO ()
setLocalizedRestaurantDescriptionText inGetAvailableRestaurantReservationBookingsIntentResponse value =
  sendMessage inGetAvailableRestaurantReservationBookingsIntentResponse setLocalizedRestaurantDescriptionTextSelector (toNSString value)

-- | @- localizedBookingAdvisementText@
localizedBookingAdvisementText :: IsINGetAvailableRestaurantReservationBookingsIntentResponse inGetAvailableRestaurantReservationBookingsIntentResponse => inGetAvailableRestaurantReservationBookingsIntentResponse -> IO (Id NSString)
localizedBookingAdvisementText inGetAvailableRestaurantReservationBookingsIntentResponse =
  sendMessage inGetAvailableRestaurantReservationBookingsIntentResponse localizedBookingAdvisementTextSelector

-- | @- setLocalizedBookingAdvisementText:@
setLocalizedBookingAdvisementText :: (IsINGetAvailableRestaurantReservationBookingsIntentResponse inGetAvailableRestaurantReservationBookingsIntentResponse, IsNSString value) => inGetAvailableRestaurantReservationBookingsIntentResponse -> value -> IO ()
setLocalizedBookingAdvisementText inGetAvailableRestaurantReservationBookingsIntentResponse value =
  sendMessage inGetAvailableRestaurantReservationBookingsIntentResponse setLocalizedBookingAdvisementTextSelector (toNSString value)

-- | @- termsAndConditions@
termsAndConditions :: IsINGetAvailableRestaurantReservationBookingsIntentResponse inGetAvailableRestaurantReservationBookingsIntentResponse => inGetAvailableRestaurantReservationBookingsIntentResponse -> IO (Id INTermsAndConditions)
termsAndConditions inGetAvailableRestaurantReservationBookingsIntentResponse =
  sendMessage inGetAvailableRestaurantReservationBookingsIntentResponse termsAndConditionsSelector

-- | @- setTermsAndConditions:@
setTermsAndConditions :: (IsINGetAvailableRestaurantReservationBookingsIntentResponse inGetAvailableRestaurantReservationBookingsIntentResponse, IsINTermsAndConditions value) => inGetAvailableRestaurantReservationBookingsIntentResponse -> value -> IO ()
setTermsAndConditions inGetAvailableRestaurantReservationBookingsIntentResponse value =
  sendMessage inGetAvailableRestaurantReservationBookingsIntentResponse setTermsAndConditionsSelector (toINTermsAndConditions value)

-- | @- availableBookings@
availableBookings :: IsINGetAvailableRestaurantReservationBookingsIntentResponse inGetAvailableRestaurantReservationBookingsIntentResponse => inGetAvailableRestaurantReservationBookingsIntentResponse -> IO (Id NSArray)
availableBookings inGetAvailableRestaurantReservationBookingsIntentResponse =
  sendMessage inGetAvailableRestaurantReservationBookingsIntentResponse availableBookingsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAvailableBookings:code:userActivity:@
initWithAvailableBookings_code_userActivitySelector :: Selector '[Id NSArray, INGetAvailableRestaurantReservationBookingsIntentCode, Id NSUserActivity] (Id INGetAvailableRestaurantReservationBookingsIntentResponse)
initWithAvailableBookings_code_userActivitySelector = mkSelector "initWithAvailableBookings:code:userActivity:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] INGetAvailableRestaurantReservationBookingsIntentCode
codeSelector = mkSelector "code"

-- | @Selector@ for @localizedRestaurantDescriptionText@
localizedRestaurantDescriptionTextSelector :: Selector '[] (Id NSString)
localizedRestaurantDescriptionTextSelector = mkSelector "localizedRestaurantDescriptionText"

-- | @Selector@ for @setLocalizedRestaurantDescriptionText:@
setLocalizedRestaurantDescriptionTextSelector :: Selector '[Id NSString] ()
setLocalizedRestaurantDescriptionTextSelector = mkSelector "setLocalizedRestaurantDescriptionText:"

-- | @Selector@ for @localizedBookingAdvisementText@
localizedBookingAdvisementTextSelector :: Selector '[] (Id NSString)
localizedBookingAdvisementTextSelector = mkSelector "localizedBookingAdvisementText"

-- | @Selector@ for @setLocalizedBookingAdvisementText:@
setLocalizedBookingAdvisementTextSelector :: Selector '[Id NSString] ()
setLocalizedBookingAdvisementTextSelector = mkSelector "setLocalizedBookingAdvisementText:"

-- | @Selector@ for @termsAndConditions@
termsAndConditionsSelector :: Selector '[] (Id INTermsAndConditions)
termsAndConditionsSelector = mkSelector "termsAndConditions"

-- | @Selector@ for @setTermsAndConditions:@
setTermsAndConditionsSelector :: Selector '[Id INTermsAndConditions] ()
setTermsAndConditionsSelector = mkSelector "setTermsAndConditions:"

-- | @Selector@ for @availableBookings@
availableBookingsSelector :: Selector '[] (Id NSArray)
availableBookingsSelector = mkSelector "availableBookings"

